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

import io.circe.*

class JsonObject(json: Json, startCache: Map[String, Any]) extends JsonObjectBase(json, startCache) {

  def selectDynamic[A](name: String)(implicit decoder: Decoder[A]): A =
    cache
      .getOrElseUpdate(
        name, {
          val ret = json.hcursor.get[A](name).getOrElse(throw MissingFieldException.default(name, json))
          ret match {
            case UndefOrUndefined(_, _) => UndefOrUndefined(Some(name), this)
            case JsonUndefined(_, _)    => JsonUndefined(Some(name), this)
            case _                      => ret
          }
        }
      )
      .asInstanceOf[A]
}

trait JsonObjectCompanion[Obj <: JsonObject] extends JsonObjectCompanionBase[Obj]
