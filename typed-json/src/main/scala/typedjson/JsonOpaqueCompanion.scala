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

import io.circe.{Codec, Decoder, Encoder}

abstract class JsonOpaqueCompanion[Underlying](
    implicit underlyingEncoder: Encoder[Underlying],
    underlyingDecoder: Decoder[Underlying]
) {
  private[typedjson] type Base
  private[typedjson] trait Tag extends Any

  type OpaqueType <: Base with Tag

  def apply(underlying: Underlying): OpaqueType = underlying.asInstanceOf[OpaqueType]

  def underlying(opaque: OpaqueType): Underlying = opaque.asInstanceOf[Underlying]

  implicit val opaqueCodec: Codec[OpaqueType] =
    Codec
      .from(underlyingDecoder, underlyingEncoder)
      .iemap[OpaqueType](u => Right(u.asInstanceOf[OpaqueType]))(o => o.asInstanceOf[Underlying])

}
