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

/*
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, unused}

import io.circe.*

class VersionedJsonObject[+For, Versions <: Version](json: Json, startCache: Map[String, Any])
    extends JsonObjectBase(json, startCache) {

  def selectDynamic[Name <: String with Singleton, V <: Versions](
      name: Name
  )(implicit @unused version: V, versionedType: VersionedType[Name, For @uncheckedVariance, V]): versionedType.A =
    cache
      .getOrElseUpdate(
        name, {
          val ret = json.hcursor
            .get[versionedType.A](name)(versionedType.decoder)
            .getOrElse(throw MissingFieldException.default(name, json))
          ret match {
            case UndefOrUndefined(_, _) => UndefOrUndefined(Some(name), this)
            case JsonUndefined(_, _)    => JsonUndefined(Some(name), this)
            case _                      => ret
          }
        }
      )
      .asInstanceOf[versionedType.A]
}

trait VersionedJsonObjectCompanion[For, Versions <: Version, Obj <: VersionedJsonObject[For, Versions]]
    extends JsonObjectCompanionBase[Obj] {

  type ThisVersionedType[Name <: String, A, V <: Versions] = VersionedType.Aux[Name, For, V, A]
  protected def makeVersionedType[Name <: String, A0, V <: Versions](
      implicit decoder0: Decoder[A0]
  ): VersionedType.Aux[Name, For, V, A0] = new VersionedType[Name, For, V] {
    override type A = A0
    override def decoder: Decoder[A0] = decoder0
  }
}

@implicitNotFound(
  "Could not find the type to use for the field ${Name} on ${For} using version ${V}. Does the field exist in the given version?"
)
trait VersionedType[Name <: String, For, V <: Version] {
  type A
  def decoder: Decoder[A]
}
object VersionedType {
  type Aux[Name <: String, For, Versions <: Version, A0] = VersionedType[Name, For, Versions] { type A = A0 }
}
 */

trait Version
