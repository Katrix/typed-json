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

import io.circe.Json

class MissingFieldException private (val message: String, val missingOn: AnyRef) extends Exception(message)
object MissingFieldException {
  def default(field: String, obj: AnyRef): MissingFieldException =
    obj match {
      case json: Json =>
        if (json.hcursor.downField(field).succeeded)
          new MissingFieldException(s"Found field $field on object $obj, but count not decode it", obj)
        else new MissingFieldException(s"Missing field $field on object $obj", obj)

      case _ => new MissingFieldException(s"Missing field $field on object $obj", obj)
    }

  def messageAndData(message: String, missingOn: AnyRef): MissingFieldException =
    new MissingFieldException(message, missingOn)
}
