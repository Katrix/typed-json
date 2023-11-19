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

import cats.data.{NonEmptyList, Validated}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.{ACursor, Decoder, HCursor}

sealed trait UndefOr[+A] {
  def isUndefined: Boolean
  def isEmpty: Boolean  = isUndefined
  def nonEmpty: Boolean = !isEmpty

  def toOption: Option[A]

  def fold[B](ifUndefined: => B)(f: A => B): B

  def map[B](f: A => B): UndefOr[B]
  def flatMap[B](f: A => UndefOr[B]): UndefOr[B]

  def filterToUndefined(f: A => Boolean): UndefOr[A]

  def contains[A1 >: A](value: A1): Boolean
  def exists[A1 >: A](f: A1 => Boolean): Boolean
  def forall[A1 >: A](f: A1 => Boolean): Boolean

  def foreach[A1 >: A](f: A1 => Unit): Unit

  def getOrElse[B >: A](other: => B): B
  def orElse[B >: A](other: => UndefOr[B]): UndefOr[B]

  def toList[A1 >: A]: List[A]

  def toEither: Either[MissingFieldException, A]
  def get: A = toEither.toTry.get
}
object UndefOr {
  implicit def undefOrDecoder[A: Decoder]: Decoder[UndefOr[A]] = new Decoder[UndefOr[A]] {

    override def apply(c: HCursor): Result[UndefOr[A]] =
      if (c.succeeded) c.as[A].map(UndefOrSome(_)) else Right(UndefOrUndefined())

    override def tryDecode(c: ACursor): Result[UndefOr[A]] =
      if (c.succeeded) c.as[A].map(UndefOrSome(_)) else Right(UndefOrUndefined())

    override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[UndefOr[A]] =
      if (c.succeeded) Validated.fromEither(c.as[A].map(UndefOrSome(_))).leftMap(NonEmptyList.one)
      else Validated.Valid(UndefOrUndefined())
  }

  def fromOption[A](opt: Option[A]): UndefOr[A] = opt match {
    case Some(value) => UndefOrSome(value)
    case None        => UndefOrUndefined()
  }

  def someIfTrue(bool: Boolean): UndefOr[Boolean] =
    if (bool) UndefOrSome(bool) else UndefOrUndefined()
}

case class UndefOrSome[A](value: A) extends UndefOr[A] {
  override def isUndefined: Boolean = false

  override def toOption: Option[A] = Some(value)

  override def fold[B](ifNull: => B)(f: A => B): B = f(value)

  override def map[B](f: A => B): UndefOr[B]              = UndefOrSome(f(value))
  override def flatMap[B](f: A => UndefOr[B]): UndefOr[B] = f(value)

  override def filterToUndefined(f: A => Boolean): UndefOr[A] = if (f(value)) UndefOrSome(value) else UndefOrUndefined()

  override def contains[A1 >: A](value: A1): Boolean      = this.value == value
  override def exists[A1 >: A](f: A1 => Boolean): Boolean = f(value)
  override def forall[A1 >: A](f: A1 => Boolean): Boolean = f(value)

  override def foreach[A1 >: A](f: A1 => Unit): Unit = f(value)

  override def getOrElse[B >: A](other: => B): B                = value
  override def orElse[B >: A](other: => UndefOr[B]): UndefOr[B] = this

  override def toList[A1 >: A]: List[A] = List(value)

  override def toEither: Either[MissingFieldException, A] = Right(value)
}

case class UndefOrUndefined(
    missingField: Option[String] = None,
    missingObj: AnyRef = null
) extends UndefOr[Nothing] {
  override def isUndefined: Boolean = true

  override def toOption: Option[Nothing] = None

  override def fold[B](ifUndefined: => B)(f: Nothing => B): B = ifUndefined

  override def map[B](f: Nothing => B): UndefOr[B]              = this
  override def flatMap[B](f: Nothing => UndefOr[B]): UndefOr[B] = this

  override def filterToUndefined(f: Nothing => Boolean): UndefOr[Nothing] = this

  override def contains[A1 >: Nothing](value: A1): Boolean      = false
  override def exists[A1 >: Nothing](f: A1 => Boolean): Boolean = false
  override def forall[A1 >: Nothing](f: A1 => Boolean): Boolean = true

  override def foreach[A1 >: Nothing](f: A1 => Unit): Unit = ()

  override def getOrElse[B >: Nothing](other: => B): B                = other
  override def orElse[B >: Nothing](other: => UndefOr[B]): UndefOr[B] = other

  override def toList[A1 >: Nothing]: List[Nothing] = Nil

  override def toEither: Either[MissingFieldException, Nothing] = Left(
    MissingFieldException.default(
      missingField.fold("unknown field")("field " + _),
      if (missingObj == null) "unknown" else missingObj
    )
  )
}
