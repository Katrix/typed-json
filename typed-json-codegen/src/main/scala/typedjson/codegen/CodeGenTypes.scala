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

package typedjson.codegen

import scala.collection.immutable.ListMap

import io.circe.*

trait CodeGenTypes {

  case class AnonymousClassTypeDef(
      imports: Seq[String],
      documentation: Option[String],
      innerTypes: Seq[TypeDef],
      allUndefined: Boolean,
      makePartial: Boolean,
      customMakeRaw: Boolean,
      fields: ListMap[String, ListMap[String, FieldDef]],
      `extends`: Seq[String],
      objectExtends: Seq[String]
  ) {
    def named(name: String): TypeDef.ClassTypeDef = TypeDef.ClassTypeDef(name, this)

    def mapFields(f: FieldDef => FieldDef): AnonymousClassTypeDef =
      copy(fields = fields.map(t1 => t1._1 -> t1._2.map(t2 => t2._1 -> f(t2._2))))
  }

  object AnonymousClassTypeDef {
    implicit lazy val typeDefDecoder: Decoder[AnonymousClassTypeDef] = (c: HCursor) => {
      implicit val fieldDefOrTypeDecoder: Decoder[Either[String, FieldDef]] =
        (c: HCursor) =>
          c.as[String] match {
            case Left(_)      => c.as[FieldDef].map(Right(_))
            case Right(value) => Right(Left(value))
          }

      for {
        imports       <- c.getOrElse[Seq[String]]("imports")(Nil)
        documentation <- c.get[Option[String]]("documentation")
        innerTypes    <- c.getOrElse[Seq[TypeDef]]("innerTypes")(Nil)
        allUndefined  <- c.getOrElse[Boolean]("allUndefined")(false)
        makePartial   <- c.getOrElse[Boolean]("makePartial")(false)
        customMakeRaw <- c.getOrElse[Boolean]("customMakeRaw")(false)
        fieldsMap     <- c.get[ListMap[String, ListMap[String, Either[String, FieldDef]]]]("fields")
        extend        <- c.getOrElse[Seq[String]]("extends")(Nil)
        objectExtends <- c.getOrElse[Seq[String]]("objectExtends")(Nil)
      } yield AnonymousClassTypeDef(
        imports,
        documentation,
        innerTypes,
        allUndefined,
        makePartial,
        customMakeRaw,
        fieldsMap.map { case (k1, v1) =>
          k1 -> v1.map { case (k2, v2) =>
            k2 -> v2.swap.map { tpe =>
              FieldDef(
                tpe,
                None,
                None,
                None,
                withUndefined = false,
                withNull = false,
                isExtension = false,
                alwaysPresent = false,
                overrides = false,
                None
              )
            }.merge
          }
        },
        extend,
        objectExtends
      )
    }
  }

  sealed trait AnonymousClassTypeDefOrType

  object AnonymousClassTypeDefOrType {
    case class TypeRef(name: String) extends AnonymousClassTypeDefOrType

    case class AnonType(anon: AnonymousClassTypeDef) extends AnonymousClassTypeDefOrType

    implicit lazy val decoder: Decoder[AnonymousClassTypeDefOrType] = (c: HCursor) =>
      c.as[String].map(TypeRef(_)).swap.map(_ => c.as[AnonymousClassTypeDef].map(AnonType(_))).swap.joinLeft
  }

  sealed trait TypeDef {
    def imports: Seq[String]
  }

  object TypeDef {
    case class ClassTypeDef(
        name: String,
        anonPart: AnonymousClassTypeDef
    ) extends TypeDef {
      override def imports: Seq[String] = anonPart.imports
    }

    case class EnumTypeDef(
        name: String,
        tpe: String,
        isBitField: Boolean,
        imports: Seq[String],
        documentation: Option[String],
        innerTypes: Seq[TypeDef],
        values: ListMap[String, EnumValue],
        objectExtends: Seq[String]
    ) extends TypeDef

    case class OpaqueTypeDef(
        name: String,
        imports: Seq[String],
        documentation: Option[String],
        underlying: String,
        includeAlias: Boolean,
        innerTypes: Seq[TypeDef],
        objectExtends: Seq[String]
    ) extends TypeDef

    case class MultipleDefs(
        imports: Seq[String],
        innerTypes: Seq[TypeDef]
    ) extends TypeDef

    case class ObjectOnlyDef(
        name: String,
        imports: Seq[String],
        innerTypes: Seq[TypeDef],
        objectExtends: Seq[String]
    ) extends TypeDef

    case class FreeformDef(
        content: String,
        imports: Seq[String],
        documentation: Option[String]
    ) extends TypeDef

    trait OtherTypeDef extends TypeDef

    implicit lazy val typeDefDecoder: Decoder[TypeDef] = (c: HCursor) =>
      for {
        imports       <- c.getOrElse[Seq[String]]("imports")(Nil)
        documentation <- c.get[Option[String]]("documentation")
        innerTypes    <- c.getOrElse[Seq[TypeDef]]("innerTypes")(Nil)
        defType       <- c.get[String]("defType")
        res <- defType match {
          case "Class" =>
            for {
              name     <- c.get[String]("name")
              anonPart <- c.as[AnonymousClassTypeDef]
            } yield ClassTypeDef(
              name,
              anonPart
            )

          case "Enum" =>
            for {
              name          <- c.get[String]("name")
              tpe           <- c.get[String]("type")
              isBitfield    <- c.getOrElse[Boolean]("isBitfield")(false)
              values        <- c.get[ListMap[String, EnumValue]]("values")
              objectExtends <- c.getOrElse[Seq[String]]("objectExtends")(Nil)
            } yield EnumTypeDef(name, tpe, isBitfield, imports, documentation, innerTypes, values, objectExtends)

          case "Opaque" =>
            for {
              name          <- c.get[String]("name")
              underlying    <- c.get[String]("underlying")
              includeAlias  <- c.getOrElse[Boolean]("includeAlias")(true)
              objectExtends <- c.getOrElse[Seq[String]]("objectExtends")(Nil)
            } yield OpaqueTypeDef(name, imports, documentation, underlying, includeAlias, innerTypes, objectExtends)

          case "Multiple" =>
            Right(MultipleDefs(imports, innerTypes))

          case "ObjectOnly" =>
            for {
              name          <- c.get[String]("name")
              objectExtends <- c.getOrElse[Seq[String]]("objectExtends")(Nil)
            } yield ObjectOnlyDef(name, imports, innerTypes, objectExtends)

          case "Freeform" =>
            for {
              content <- c.get[String]("content")
            } yield FreeformDef(content, imports, documentation)

          case other => decodeOtherTypeDef(other, c)
        }
      } yield res
  }

  protected def decodeOtherTypeDef(other: String, c: HCursor): Decoder.Result[TypeDef.OtherTypeDef]

  case class FieldDef(
      tpe: String,
      jsonName: Option[String],
      default: Option[String],
      documentation: Option[String],
      withUndefined: Boolean,
      withNull: Boolean,
      isExtension: Boolean,
      alwaysPresent: Boolean,
      overrides: Boolean,
      verification: Option[FieldVerification]
  )

  object FieldDef {

    implicit lazy val fieldDefDecoder: Decoder[FieldDef] = (c: HCursor) =>
      for {
        tpe           <- c.get[String]("type")
        jsonName      <- c.get[Option[String]]("jsonName")
        default       <- c.get[Option[String]]("default")
        documentation <- c.get[Option[String]]("documentation")
        withUndefined <- c.getOrElse[Boolean]("withUndefined")(false)
        withNull      <- c.getOrElse[Boolean]("withNull")(false)
        isExtension   <- c.getOrElse[Boolean]("isExtension")(false)
        alwaysPresent <- c.getOrElse[Boolean]("alwaysPresent")(false)
        overrides     <- c.getOrElse[Boolean]("override")(false)
        verification  <- c.get[Option[FieldVerification]]("verification")
      } yield FieldDef(
        tpe,
        jsonName,
        default,
        documentation,
        withUndefined,
        withNull,
        isExtension,
        alwaysPresent,
        overrides,
        verification
      )
  }

  case class FieldVerification(
      minLength: Option[Int],
      maxLength: Option[Int]
  )

  object FieldVerification {
    implicit lazy val fieldVerificationDecoder: Decoder[FieldVerification] = (c: HCursor) =>
      for {
        minLength <- c.get[Option[Int]]("minLength")
        maxLength <- c.get[Option[Int]]("maxLength")
      } yield FieldVerification(minLength, maxLength)
  }

  case class EnumValue(value: String, documentation: Option[String])
  object EnumValue {
    implicit lazy val enumValueDecoder: Decoder[EnumValue] = (c: HCursor) =>
      c.as[String]
        .map(EnumValue(_, None))
        .swap
        .map[Either[DecodingFailure, EnumValue]] { _ =>
          for {
            value         <- c.get[String]("value")
            documentation <- c.get[Option[String]]("documentation")
          } yield EnumValue(value, documentation)
        }
        .swap
        .joinLeft
  }
}
