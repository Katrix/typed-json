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

import java.nio.file.{Files, Path}

import io.circe.*

trait TypedJsonCodeGen {
  val codeGenTypes: CodeGenTypes
  import codeGenTypes.*

  def isUndefined(allUndefined: Boolean, withUndefined: Boolean, alwaysPresent: Boolean): Boolean =
    !alwaysPresent && (allUndefined || withUndefined)

  def isFieldUndefined(field: FieldDef, allUndefined: Boolean): Boolean =
    isUndefined(allUndefined, field.withUndefined, field.alwaysPresent)

  def generateCodeFromFile(generatedRoot: Path, yamlFile: Path): String = {
    val relativeYamlFile = generatedRoot.relativize(yamlFile)

    val relativeYamlPath = Compat.javaIteratorToScalaIterator(relativeYamlFile.iterator).map(_.toString).toList.init

    val typeDef =
      yaml.parser.parse(Compat.javaListToScala(Files.readAllLines(yamlFile)).mkString("\n")).flatMap(_.as[TypeDef]).toTry.get

    val packageLoc = relativeYamlPath.mkString(".")
    val extraImports =
      automaticImports(typeDef)
        .filter(s => if (s.endsWith("._")) s.substring(0, s.length - 2) != packageLoc else true)
        .distinct

    GenAST.printFile(
      GenAST.ScalaFile(
        packageLoc = packageLoc,
        intelliJIgnoredInspections = Seq("ScalaWeakerAccess", "ScalaUnusedSymbol", "DuplicatedCode"),
        disclaimer = s"""|THIS FILE IS MACHINE GENERATED!
              |
              |Do not edit this file directly.
              |Instead, edit the file generated/${relativeYamlPath.mkString("/")}/${yamlFile.getFileName.toString}
              |""".stripMargin,
        definitions = codeFromTypeDef(typeDef, extraImports = extraImports)
      )
    )
  }

  def automaticImports(typeDef: TypeDef): Seq[String] = typeDef match {
    case td: TypeDef.ClassTypeDef =>
      Seq("io.circe.Json", "typedjson._") ++ td.anonPart.innerTypes.flatMap(automaticImports)
    case td: TypeDef.EnumTypeDef                => Seq("typedjson._") ++ td.innerTypes.flatMap(automaticImports)
    case td: TypeDef.OpaqueTypeDef              => Seq("typedjson._") ++ td.innerTypes.flatMap(automaticImports)
    case td: TypeDef.MultipleDefs               => td.innerTypes.flatMap(automaticImports)
    case td: TypeDef.ObjectOnlyDef              => td.innerTypes.flatMap(automaticImports)
    case _: TypeDef.FreeformDef                 => Nil
    case other: TypeDef.OtherTypeDef @unchecked => automaticImportsOtherTypedef(other)
  }

  protected def automaticImportsOtherTypedef(typeDef: TypeDef.OtherTypeDef): Seq[String]

  def codeFromTypeDef(typeDef: TypeDef, extraImports: Seq[String] = Nil): List[GenAST.Definition] = {
    val res: List[GenAST.Definition] = typeDef match {
      case classTypeDef: TypeDef.ClassTypeDef     => List(codeFromClassTypeDef(classTypeDef))
      case enumTypeDef: TypeDef.EnumTypeDef       => List(codeFromEnumTypeDef(enumTypeDef))
      case opaqueTypeDef: TypeDef.OpaqueTypeDef   => List(codeFromOpaqueTypeDef(opaqueTypeDef))
      case multiple: TypeDef.MultipleDefs         => multiple.innerTypes.toList.flatMap(codeFromTypeDef(_, Nil))
      case objectOnly: TypeDef.ObjectOnlyDef      => List(codeFromObjectOnly(objectOnly))
      case freeform: TypeDef.FreeformDef          => List(codeFromFreeform(freeform))
      case other: TypeDef.OtherTypeDef @unchecked => codeFromOtherTypeDef(other)
    }

    GenAST.Imports(extraImports ++ typeDef.imports) :: res
  }

  protected def codeFromOtherTypeDef(typeDef: TypeDef.OtherTypeDef): List[GenAST.Definition]

  def camelCaseFromSnakecase(s: String): String = {
    val arr = s.split("_")
    arr.head + arr.iterator.drop(1).map(_.capitalize).mkString
  }

  def pascalCaseFromSnakecase(s: String): String = {
    val base = camelCaseFromSnakecase(s)
    base.charAt(0).toUpper.toString + base.substring(1)
  }

  case class FieldWithType(field: FieldDef, tpe: String)
  case class FieldWithTypeVersionAndName(field: FieldDef, tpe: String, version: Int, name: String)

  def codeFromClassTypeDef(classTypeDef: TypeDef.ClassTypeDef): GenAST.Definition = {
    val tpeName      = classTypeDef.name
    val allUndefined = classTypeDef.anonPart.allUndefined

    val fieldsWithTypes = classTypeDef.anonPart.fields.map { case (version, fields) =>
      version -> fields.map { case (name, field) =>
        val fieldType = (isFieldUndefined(field, allUndefined), field.withNull) match {
          case (true, true)   => s"JsonOption[${field.tpe}]"
          case (true, false)  => s"UndefOr[${field.tpe}]"
          case (false, true)  => s"Option[${field.tpe}]"
          case (false, false) => field.tpe
        }

        name -> FieldWithType(field, fieldType)
      }
    }

    val makeRaw =
      if (classTypeDef.anonPart.customMakeRaw) Nil
      else
        List(
          GenAST.DefDef(
            name = "makeRaw",
            parameters = Seq(
              Seq(
                GenAST.Parameter(name = "json", tpe = "Json"),
                GenAST.Parameter(name = "cache", tpe = "Map[String, Any]")
              )
            ),
            returnType = tpeName,
            rhs = Some(GenAST.FreeformExpr(s"new $tpeName(json, cache)"))
          )
        )

    val makeDefs = fieldsWithTypes.map { case (version, fields) =>
      val defName = s"make${version.replace("x", "").replace(".", "")}"

      val params = fields.map { case (field, FieldWithType(fieldInfo, tpe)) =>
        val undef       = isFieldUndefined(fieldInfo, allUndefined)
        val realDefault = if (undef) fieldInfo.default.orElse(Some("undefined")) else fieldInfo.default

        GenAST.Parameter(
          docs = fieldInfo.documentation,
          name = camelCaseFromSnakecase(field),
          tpe = tpe,
          default = realDefault
            .map { s =>
              (s, undef, fieldInfo.withNull) match {
                case ("null", true, true)       => "JsonNull"
                case ("undefined", true, true)  => s"""JsonUndefined(Some("$field"))"""
                case ("null", false, true)      => "None"
                case ("undefined", true, false) => s"""UndefOrUndefined(Some("$field"))"""
                case (d, true, true)            => s"JsonSome($d)"
                case (d, false, true)           => s"Some($d)"
                case (d, true, false)           => s"UndefOrSome($d)"
                case (d, false, false)          => d
              }
            }
            .map(s => GenAST.FreeformExpr(s))
        )
      }.toSeq

      val args = fields.map { case (field, FieldWithType(fieldInfo, _)) =>
        val jsonName = fieldInfo.jsonName.getOrElse(field)
        val fieldLit = "\"" + jsonName + "\""
        if (fieldInfo.isExtension) {
          GenAST.FunctionCall(
            "JsonObjectFrom.FromExtension",
            Nil,
            Seq(Seq(GenAST.FreeformExpr(fieldLit), GenAST.FreeformExpr(camelCaseFromSnakecase(field))))
          )
        } else {
          if (isFieldUndefined(fieldInfo, allUndefined))
            GenAST.FreeformExpr(s"$fieldLit :=? ${camelCaseFromSnakecase(field)}")
          else GenAST.FreeformExpr(s"$fieldLit := ${camelCaseFromSnakecase(field)}")
        }
      }.toSeq

      GenAST.DefDef(
        name = defName,
        parameters = Seq(params),
        returnType = tpeName,
        rhs = Some(GenAST.FunctionCall("makeRawFromFields", Nil, Seq(args)))
      )
    }.toSeq

    val allClassFields = fieldsWithTypes.toSeq.flatMap { case (version, fields) =>
      val intVersion = version.replace("x", "").replace(".", "").toInt
      fields.map(field => FieldWithTypeVersionAndName(field._2.field, field._2.tpe, intVersion, field._1))
    }
    val highestVersionAll = fieldsWithTypes.keys.map(s => s.replace("x", "").replace(".", "").toInt).max

    val groupedClassFields = allClassFields.zipWithIndex
      .groupBy(_._1.name)
      .map { case (name, fields) =>
        val (field, idx) = fields.maxBy(_._1.version)
        name -> (field, idx)
      }
      .toSeq
      .sortBy(_._2._2)
      .map { case (_, v) => v._1 }

    val classDefs = groupedClassFields.flatMap {
      case FieldWithTypeVersionAndName(fieldDef, tpe, highestVersion, name) =>
        val undef = isFieldUndefined(fieldDef, allUndefined)
        val baseMods =
          if (highestVersion < highestVersionAll)
            List(
              "@inline",
              s"""@deprecated(message = "Value might be missing", since = "${highestVersionAll.toString}")""",
              s"private[ackcord]"
            )
          else List("@inline")

        val accessorDefMods = if (fieldDef.overrides) baseMods :+ "override" else baseMods

        val jsonName = fieldDef.jsonName.getOrElse(name)

        Seq(
          GenAST.DefDef(
            docs = fieldDef.documentation,
            mods = accessorDefMods,
            name = camelCaseFromSnakecase(name),
            returnType = tpe,
            rhs = Some(
              if (fieldDef.isExtension)
                GenAST.FunctionCall(
                  s"$tpe.makeRaw",
                  Nil,
                  Seq(
                    Seq(
                      GenAST.FreeformExpr("json"),
                      GenAST.FunctionCall("extensionCache", Nil, Seq(Seq(GenAST.FreeformExpr("\"" + jsonName + "\""))))
                    )
                  )
                )
              else GenAST.FunctionCall("selectDynamic", Seq(tpe), Seq(Seq(GenAST.FreeformExpr("\"" + jsonName + "\""))))
            )
          ),
          GenAST.DefDef(
            mods = baseMods,
            name = s"with${pascalCaseFromSnakecase(name)}",
            parameters = Seq(Seq(GenAST.Parameter(name = "newValue", tpe = tpe))),
            returnType = tpeName,
            rhs = Some(
              if (fieldDef.isExtension)
                GenAST.FunctionCall(
                  "objWithJson",
                  Nil,
                  Seq(
                    Seq(
                      GenAST.FreeformExpr(tpeName),
                      GenAST.FreeformExpr("newValue.json"),
                      GenAST.FreeformExpr("newValue.cacheCopy")
                    )
                  )
                )
              else
                GenAST.FunctionCall(
                  if (undef) "objWithUndef" else "objWith",
                  Nil,
                  Seq(
                    Seq(
                      GenAST.FreeformExpr(tpeName),
                      GenAST.FreeformExpr("\"" + jsonName + "\""),
                      GenAST.FreeformExpr("newValue")
                    )
                  )
                )
            )
          )
        )
    }

    val values = groupedClassFields.map(t => GenAST.FreeformExpr(s"() => ${camelCaseFromSnakecase(t.name)}"))

    val partialDef =
      if (classTypeDef.anonPart.makePartial)
        List(classTypeDef.anonPart.copy(allUndefined = true, makePartial = false, innerTypes = Nil).named("Partial"))
      else Nil
    val allInnerTypes = partialDef ++ classTypeDef.anonPart.innerTypes

    val classCode = GenAST.Class(
      docs = classTypeDef.anonPart.documentation,
      name = tpeName,
      constructors = Seq(
        GenAST.Constructor(parameters =
          Seq(
            Seq(
              GenAST.Parameter(name = "json", tpe = "Json"),
              GenAST
                .Parameter(name = "cache", tpe = "Map[String, Any]", default = Some(GenAST.FreeformExpr("Map.empty")))
            )
          )
        )
      ),
      extend = "JsonObject(json, cache)" +: classTypeDef.anonPart.`extends`,
      members = classDefs :+ GenAST.DefDef(
        mods = Seq("override"),
        name = "values",
        returnType = "Seq[() => Any]",
        rhs = Some(
          GenAST.FunctionCall("Seq", Nil, Seq(values))
        )
      )
    )

    val companionCode = GenAST.Module(
      name = tpeName,
      extend = s"JsonObjectCompanion[$tpeName]" +: classTypeDef.anonPart.objectExtends,
      members = makeRaw ++ makeDefs ++ allInnerTypes.flatMap(codeFromTypeDef(_))
    )

    GenAST.Grouped(classCode, companionCode)
  }

  def codeFromObjectOnly(objectOnlyDef: TypeDef.ObjectOnlyDef): GenAST.Definition =
    GenAST.Module(
      name = objectOnlyDef.name,
      extend = objectOnlyDef.objectExtends,
      members = objectOnlyDef.innerTypes.flatMap(codeFromTypeDef(_))
    )

  def codeFromFreeform(freeformDef: TypeDef.FreeformDef): GenAST.Definition =
    GenAST.FreeformDefinition(freeformDef.documentation, freeformDef.content)

  def codeFromEnumTypeDef(enumTypeDef: TypeDef.EnumTypeDef): GenAST.Definition = {
    val tpeName        = enumTypeDef.name
    val underlyingType = enumTypeDef.tpe

    def wrap(value: String): String = underlyingType match {
      case "String" => "\"" + value + "\""
      case _        => value
    }

    val classCode = GenAST.Class(
      docs = enumTypeDef.documentation,
      mods = Seq("sealed", "case"),
      name = tpeName,
      constructors = Seq(
        GenAST.Constructor(
          mods = Seq("private"),
          parameters = Seq(Seq(GenAST.Parameter(name = "value", tpe = underlyingType)))
        )
      ),
      extend = Seq(s"JsonEnum[$underlyingType]")
    )

    val bitfieldMember =
      if (enumTypeDef.isBitField)
        Seq(
          GenAST.Class(
            mods = Seq("implicit"),
            name = s"${tpeName}BitFieldOps",
            constructors = Seq(
              GenAST.Constructor(parameters =
                Seq(Seq(GenAST.Parameter(mods = Seq("private", "val"), name = "here", tpe = tpeName)))
              )
            ),
            extend = Seq("AnyVal"),
            members = Seq(
              GenAST.DefDef(
                name = s"to$underlyingType",
                returnType = underlyingType,
                rhs = Some(GenAST.FreeformExpr("here.value"))
              ),
              GenAST.DefDef(
                name = "++",
                parameters = Seq(Seq(GenAST.Parameter(name = "there", tpe = tpeName))),
                returnType = tpeName,
                rhs = Some(GenAST.FreeformExpr(s"$tpeName(here.value | there.value)"))
              ),
              GenAST.DefDef(
                name = "--",
                parameters = Seq(Seq(GenAST.Parameter(name = "there", tpe = tpeName))),
                returnType = tpeName,
                rhs = Some(GenAST.FreeformExpr(s"$tpeName(here.value & ~there.value)"))
              ),
              GenAST.DefDef(
                name = s"isNone",
                returnType = "Boolean",
                rhs = Some(GenAST.FreeformExpr("here.value == 0"))
              )
            )
          )
        )
      else Nil

    val companionCode = GenAST.Module(
      name = tpeName,
      extend = Seq(s"JsonEnumCompanion[$underlyingType, $tpeName]") ++ enumTypeDef.objectExtends,
      members = (enumTypeDef.values.map { case (name, value) =>
        GenAST.ValDef(
          docs = value.documentation,
          name = name,
          returnType = tpeName,
          rhs = Some(GenAST.FreeformExpr(s"$tpeName(${wrap(value.value)})"))
        )
      }.toSeq :+ GenAST.DefDef(
        name = "unknown",
        parameters = Seq(Seq(GenAST.Parameter(name = "value", tpe = underlyingType))),
        returnType = tpeName,
        rhs = Some(GenAST.FreeformExpr(s"new $tpeName(value)"))
      ) :+ GenAST.ValDef(
        name = "values",
        returnType = s"Seq[$tpeName]",
        rhs = Some(GenAST.FunctionCall("Seq", Nil, Seq(enumTypeDef.values.keys.map(GenAST.FreeformExpr(_)).toSeq)))
      )) ++ bitfieldMember ++ enumTypeDef.innerTypes.flatMap(codeFromTypeDef(_))
    )

    GenAST.Grouped(classCode, companionCode)
  }

  def codeFromOpaqueTypeDef(opaqueTypeDef: TypeDef.OpaqueTypeDef): GenAST.Definition = {
    val tpeName = opaqueTypeDef.name

    val companion = GenAST.Module(
      docs = opaqueTypeDef.documentation,
      name = tpeName,
      extend = Seq(s"JsonOpaqueCompanion[${opaqueTypeDef.underlying}]") ++ opaqueTypeDef.objectExtends,
      members = Seq(
        GenAST.TypeDef(name = tpeName, rhs = Some("OpaqueType"))
      ) ++ opaqueTypeDef.innerTypes.flatMap(codeFromTypeDef(_))
    )

    if (opaqueTypeDef.includeAlias)
      GenAST.Grouped(GenAST.TypeDef(name = tpeName, rhs = Some(s"$tpeName.$tpeName")), companion)
    else companion
  }
}
