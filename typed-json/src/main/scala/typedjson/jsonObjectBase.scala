/*
 * This file is part of typed-json, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2023 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package typedjson

import scala.language.implicitConversions

import io.circe.*
import io.circe.syntax.*
import scala.collection.compat.*

class JsonObjectBase(val json: Json, startCache: Map[String, Any]) {

  private[typedjson] val cache = collection.mutable.Map.from(startCache)

  def cacheCopy: Map[String, Any] = cache.toMap

  def extensionCache(s: String): Map[String, Any] = startCache.collect {
    case (k, v) if k.startsWith(s"$s.") => k.substring(s.length + 1) -> v
  }

  def objWith[A <: JsonObjectBase, V](companion: JsonObjectCompanionBase[A], name: String, obj: V)(
      implicit encoder: Encoder[V]
  ): A = objWithJson(companion, Json.obj(name -> encoder(obj)), Map(name -> obj))

  def objWithUndef[A <: JsonObjectBase, V](
      companion: JsonObjectCompanionBase[A],
      name: String,
      obj: UndefOr[V]
  )(
      implicit encoder: Encoder[V]
  ): A = obj match {
    case UndefOrSome(value)     => objWith[A, V](companion, name, value)
    case UndefOrUndefined(_, _) => objWithout[A](companion, name)
  }

  def objWithUndef[A <: JsonObjectBase, V](
      companion: JsonObjectCompanionBase[A],
      name: String,
      obj: JsonOption[V]
  )(
      implicit encoder: Encoder[V]
  ): A = obj match {
    case JsonSome(value)     => objWith[A, V](companion, name, value)
    case JsonNull            => objWith[A, Option[V]](companion, name, None)
    case JsonUndefined(_, _) => objWithout[A](companion, name)
  }

  def objWithJson[A <: JsonObjectBase](
      companion: JsonObjectCompanionBase[A],
      json: Json,
      cacheUpdates: Map[String, Any]
  ): A = companion.makeRaw(json.deepMerge(json), cacheCopy ++ cacheUpdates)

  def objWithout[A <: JsonObjectBase](companion: JsonObjectCompanionBase[A], name: String): A =
    companion.makeRaw(json.mapObject(_.remove(name)), cacheCopy - name)

  def retype[A <: JsonObjectBase](companion: JsonObjectCompanionBase[A]): A =
    companion.makeRaw(json, Map.empty)

  override def toString = s"${getClass.getSimpleName}($json)"

  def values: Seq[() => Any] = Nil
}
trait JsonObjectCompanionBase[Obj <: JsonObjectBase] {

  implicit val codec: Codec[Obj] = Codec.from(
    Decoder[Json].map(makeRaw(_, Map.empty)),
    Encoder[Json].contramap[Obj](_.json)
  )

  def makeRaw(json: Json, cache: Map[String, Any]): Obj

  def makeRawFromFields(fields: JsonObjectFrom*): Obj = {
    val jsonFields = fields.flatMap {
      case f: JsonObjectFrom.MakeField => f.json.map(f.fieldName -> _).toList
      case JsonObjectFrom.FromExtension(_, obj) =>
        obj.json.asObject.get.toList
    }

    val json = Json.obj(jsonFields: _*)
    val cache = fields.flatMap {
      case f: JsonObjectFrom.MakeField => List(f.fieldName -> f.value)
      case JsonObjectFrom.FromExtension(fieldName, obj) =>
        obj.cache.map(t => (fieldName + "." + t._1) -> t._2)
    }.toMap

    makeRaw(json, cache)
  }

  implicit def makeFieldOps(s: String): MakeFieldOps = new MakeFieldOps(s)
}

class MakeFieldOps(private val str: String) extends AnyVal {
  import JsonObjectFrom.MakeField
  def :=[A](value: A)(implicit encoder: Encoder[A]): MakeField =
    MakeField.MakeFieldImpl(str, value, Some(encoder(value)))

  def :=?[A](value: UndefOr[A])(implicit encoder: Encoder[A]): MakeField =
    MakeField.MakeFieldImpl(str, value, value.toOption.map(encoder(_)))

  def :=?[A](value: JsonOption[A])(implicit encoder: Encoder[A]): MakeField =
    MakeField.MakeFieldImpl(str, value, if (value.isUndefined) None else Some(value.toOption.asJson))
}

sealed trait JsonObjectFrom
object JsonObjectFrom {
  sealed trait MakeField extends JsonObjectFrom {
    type A
    val fieldName: String
    val value: A
    val json: Option[Json]
  }

  case class FromExtension(fieldName: String, obj: JsonObjectBase) extends JsonObjectFrom

  object MakeField {
    case class MakeFieldImpl[A0](fieldName: String, value: A0, json: Option[Json]) extends MakeField {
      type A = A0
    }
  }
}
