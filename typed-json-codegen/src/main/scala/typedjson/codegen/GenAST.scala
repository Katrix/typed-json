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

import cats.data.Chain
import typedjson.codegen.CodePrinterSegment as Segment

object GenAST {

  // Drop this when we drop support for Scala 2.12
  private def optionWhen[A](cond: Boolean)(a: => A) = if (cond) Some(a) else None

  case class ScalaFile(
      packageLoc: String,
      intelliJIgnoredInspections: Seq[String],
      disclaimer: String,
      definitions: Seq[Definition]
  )

  sealed trait Definition

  case class Imports(
      imports: Seq[String]
  ) extends Definition

  case class Grouped(
      definitions: Definition*
  ) extends Definition

  case class Class(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      typeParameters: Seq[TypeParameter] = Nil,
      constructors: Seq[Constructor] = Nil,
      extend: Seq[String] = Nil,
      members: Seq[Definition] = Nil,
      isTrait: Boolean = false
  ) extends Definition
  case class Module(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      extend: Seq[String] = Nil,
      members: Seq[Definition] = Nil
  ) extends Definition

  case class DefDef(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      typeParameters: Seq[TypeParameter] = Nil,
      parameters: Seq[Seq[Parameter]] = Nil,
      implicitParameters: Seq[Parameter] = Nil,
      returnType: String,
      rhs: Option[Expr] = None
  ) extends Definition
  case class ValDef(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      returnType: String,
      rhs: Option[Expr] = None
  ) extends Definition
  case class TypeDef(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      upperBound: Option[String] = None,
      lowerBound: Option[String] = None,
      rhs: Option[String] = None
  ) extends Definition
  case class FreeformDefinition(
      documentation: Option[String] = None,
      content: String
  ) extends Definition

  case class Constructor(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      parameters: Seq[Seq[Parameter]] = Nil,
      implicitParamters: Seq[Parameter] = Nil,
      rhs: Option[Expr] = None
  )

  case class Parameter(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      tpe: String,
      default: Option[Expr] = None
  )

  case class TypeParameter(
      docs: Option[String] = None,
      mods: Seq[String] = Nil,
      name: String,
      upperBound: Option[String] = None,
      lowerBound: Option[String] = None
  )

  sealed trait Expr                                                                            extends Definition
  case class Block(statements: Seq[Definition], last: Expr)                                    extends Expr
  case class FunctionCall(function: String, typeArgs: Seq[String], argss: Seq[Seq[Expr]])      extends Expr
  case class AssignExpr(lhs: String, rhs: Expr)                                                extends Expr
  case class FreeformExpr(code: String)                                                        extends Expr
  case class NewExpr(extend: String, withs: Seq[String] = Nil, members: Seq[Definition] = Nil) extends Expr

  def printFile(file: ScalaFile): String = {
    s"""|//noinspection ${file.intelliJIgnoredInspections.mkString(", ")}
        |package ${file.packageLoc}
        |
        |${file.disclaimer.linesIterator.mkString("// ", "\n// ", "")}
        |
        |${CodePrinter.print(printDefinitions(file.definitions).flatMap(identity).toList).mkString("\n")}""".stripMargin
  }

  private def printDefinitions(definitions: Seq[Definition]): Chain[Chain[Segment]] =
    Chain.fromSeq(definitions).map { definition =>
      val segments = printDefinition(definition)
      if (segments.nonEmpty) segments :+ Segment.Newline else segments
    }

  private def spaced(content: String): Chain[Segment] =
    Chain(Segment.Space, Segment.Content(content), Segment.Space)

  private def printDefinition(definition: Definition): Chain[Segment] = {
    definition match {
      case Imports(imports) =>
        if (imports.isEmpty) Chain.empty
        else
          Chain.fromSeq(imports).flatMap(s => Chain(Segment.Content("import " + s), Segment.NewlineIfNotAlreadyPrinted))

      case Grouped(definitions*) =>
        val printed = Chain.fromSeq(definitions).map(printDefinition)
        printed.initLast.fold(Chain.empty[Segment]) { case (init, last) =>
          (init.map(_ :+ Segment.NewlineIfNotAlreadyPrinted) :+ last).flatMap(identity)
        }

      case Class(docs, mods, name, typeParameters, constructors, extend, members, isTrait) =>
        val paramsMods     = constructors.headOption.toSeq.flatMap(_.mods)
        val paramModsChain = if (paramsMods.nonEmpty) Segment.Space +: printMods(paramsMods) else Chain.empty

        val memberChain =
          if (members.nonEmpty || constructors.length > 1) {
            val ctor = Chain
              .fromSeq(constructors.drop(1))
              .map { c =>
                makeDocs(c.docs) ++ printMods(c.mods) ++ Chain(
                  Segment.Content("def"),
                  Segment.Space,
                  Segment.Content("this")
                ) ++ printParameters(Nil, c.parameters, c.implicitParamters) ++ printRhs(
                  Some(c.rhs.get)
                )
              }
              .map(_.toList)

            Chain(
              Segment.Space,
              Segment.simpleBlock("{", (Chain(Nil) ++ ctor ++ printDefinitions(members).map(_.toList)).toList, "}"),
              Segment.NewlineIfNotAlreadyPrinted
            )
          } else Chain.empty

        makeDocs(
          docs,
          types = typeParameters.map(t => t.name -> t.docs),
          params = constructors.headOption.toSeq.flatMap { c =>
            (c.parameters.flatten ++ c.implicitParamters).map(p => p.name -> p.docs)
          }
        ) ++ printMods(mods) ++ Chain(
          Segment.Content(if (isTrait) "trait" else "class"),
          Segment.Space,
          Segment.Content(name)
        ) ++ paramModsChain ++ printParameters(
          typeParameters,
          constructors.headOption.map(_.parameters).getOrElse(Nil),
          constructors.headOption.map(_.implicitParamters).getOrElse(Nil)
        ) ++ printExtends(extend) ++ memberChain

      case Module(docs, mods, name, extend, members) =>
        makeDocs(docs) ++ printMods(mods) ++ Chain(
          Segment.Content("object"),
          Segment.Space,
          Segment.Content(name)
        ) ++ printExtends(extend) :+ Segment.Space :+ Segment.simpleBlock(
          "{",
          printDefinitions(members).map(_.toList).toList,
          "}"
        ) :+ Segment.NewlineIfNotAlreadyPrinted

      case DefDef(docs, mods, name, typeParameters, parameters, implicitParameters, returnType, rhs) =>
        makeDocs(
          docs,
          types = typeParameters.map(t => t.name -> t.docs),
          params = (parameters.flatten ++ implicitParameters).map(p => p.name -> p.docs)
        ) ++ printMods(mods) ++ Chain(Segment.Content("def"), Segment.Space, Segment.Content(name)) ++ printParameters(
          typeParameters,
          parameters,
          implicitParameters
        ) ++ Chain(
          Segment.Content(":"),
          Segment.Space,
          Segment.Content(returnType)
        ) ++ printRhs(rhs)

      case ValDef(docs, mods, name, returnType, rhs) =>
        makeDocs(docs) ++ printMods(mods) ++
          Chain(
            Segment.Content("val"),
            Segment.Space,
            Segment.Content(name + ":"),
            Segment.Space,
            Segment.Content(returnType)
          ) ++ printRhs(rhs)

      case TypeDef(docs, mods, name, upperBound, lowerBound, rhs) =>
        val upper = upperBound.fold(Chain.empty[Segment])(t => spaced(">:") :+ Segment.Content(t))
        val lower = lowerBound.fold(Chain.empty[Segment])(t => spaced("<:") :+ Segment.Content(t))
        val equal = rhs.fold(Chain.empty[Segment])(t => spaced("=") :+ Segment.Content(t))

        makeDocs(docs) ++ printMods(mods) ++
          Chain(Segment.Content("type"), Segment.Space, Segment.Content(name)) ++ lower ++ upper ++ equal

      case FreeformDefinition(docs, content) =>
        makeDocs(docs) ++ Chain(
          Segment.NewlineIfNotAlreadyPrinted
        ) ++ Chain
          .fromIterableOnce(content.linesIterator)
          .flatMap(c => Chain(Segment.Content(c), Segment.Newline)) ++ Chain(
          Segment.NewlineIfNotAlreadyPrinted
        )

      case e: Expr => printExpr(e)
    }
  }

  private def makeDocs(
      docs: Option[String],
      types: Seq[(String, Option[String])] = Nil,
      params: Seq[(String, Option[String])] = Nil
  ): Chain[Segment] = {
    def onlySomeDocs(seq: Seq[(String, Option[String])]): Seq[(String, String)] =
      seq.flatMap(t => t._2.toSeq.map(d => t._1 -> d))

    def param(tpe: String, name: String, docs: String) = List(
      List(Segment.Content(tpe), Segment.Space, Segment.Content(name)),
      List(Segment.Space, Segment.Space, Segment.Content(docs))
    )

    val typesChain  = onlySomeDocs(types).toList.flatMap(t => param("@tparam", t._1, t._2))
    val paramsChain = onlySomeDocs(params).toList.flatMap(t => param("@param", t._1, t._2))

    if (docs.nonEmpty || typesChain.nonEmpty || paramsChain.nonEmpty)
      Chain(
        Segment.ParameterBlock(
          onlyPrePostLineOnMultiline = true,
          List(Segment.Content("/**"), Segment.Space),
          List(Segment.Content("*"), Segment.Space),
          List(
            docs.map(s => List(List(Segment.Content(s)))),
            optionWhen(typesChain.nonEmpty || paramsChain.nonEmpty)(Nil: List[List[Segment]]),
            optionWhen(typesChain.nonEmpty)(typesChain),
            optionWhen(paramsChain.nonEmpty)(paramsChain)
          ).flatMap(_.toList).flatten,
          Nil,
          List(Segment.Space, Segment.Space, Segment.Content("*/"))
        ),
        Segment.NewlineIfNotAlreadyPrinted
      )
    else Chain.empty
  }

  private def printMods(mods: Seq[String]): Chain[Segment] = {
    val (annotations, otherMods) = mods.partition(_.startsWith("@"))
    val allMods                  = (Chain.fromSeq(annotations) ++ Chain.fromSeq(otherMods)).map(Segment.Content(_))
    allMods.uncons.fold(Chain.empty[Segment]) { case (head, tail) =>
      head +: tail.flatMap(s => Chain(Segment.Space, s)) :+ Segment.Space
    }
  }

  private def printWiths(withs: Seq[String]): Chain[Segment] = {
    def part(keyword: String)(t: String) =
      spaced(keyword) :+ Segment.Content(t)

    Chain.fromSeq(withs).flatMap(part("with"))
  }

  private def printExtends(extend: Seq[String]): Chain[Segment] = {
    def part(keyword: String)(t: String) =
      spaced(keyword) :+ Segment.Content(t)

    Chain.fromOption(extend.headOption).flatMap(part("extends")) ++ printWiths(extend.drop(1))
  }

  private def printParameters(
      typeParameters: Seq[TypeParameter],
      parameters: Seq[Seq[Parameter]],
      implicitParameters: Seq[Parameter]
  ): Chain[Segment] = {
    def printTypeParameter(tparam: TypeParameter): Chain[Segment] = {
      val upper = tparam.upperBound.fold(Chain.empty[Segment])(t => spaced(">:") :+ Segment.Content(t))
      val lower = tparam.lowerBound.fold(Chain.empty[Segment])(t => spaced(">:") :+ Segment.Content(t))
      (printMods(tparam.mods) :+ Segment.Content(tparam.name)) ++ upper ++ lower
    }

    def printParameter(param: Parameter): Chain[Segment] =
      printMods(param.mods) ++
        Chain(Segment.Content(param.name + ":"), Segment.Space, Segment.Content(param.tpe)) ++
        printRhs(param.default)

    def printParameterBlock(block: Seq[Parameter]): Segment.ParameterBlock =
      Segment.simpleParameterBlock("(", block.toList.map(printParameter(_).toList), ")")

    val types = Segment.simpleParameterBlock("[", typeParameters.toList.map(printTypeParameter(_).toList), "]")

    val allParameters =
      if (implicitParameters.nonEmpty)
        parameters :+ implicitParameters.zipWithIndex.map(t =>
          if (t._2 == 0) t._1.copy(mods = "implicit" +: t._1.mods) else t._1
        )
      else parameters

    types +: Chain.fromSeq(allParameters.map(printParameterBlock))
  }

  private def printRhs(rhs: Option[Expr]): Chain[Segment] = rhs.fold(Chain.empty[Segment]) { expr =>
    spaced("=") :+ Segment.NewlineIfNotEnoughSpace(printExpr(expr).toList)
  }

  private def printExpr(expr: Expr): Chain[Segment] = expr match {
    case Block(statements, last) =>
      Chain(
        Segment.Space,
        Segment
          .simpleBlock("{", (printDefinitions(statements).map(_.toList) ++ printExpr(last).map(List(_))).toList, "}"),
        Segment.NewlineIfNotAlreadyPrinted
      )

    case FunctionCall(function, typeArgs, argss) =>
      Chain(
        Segment.Content(function),
        Segment.simpleParameterBlock("[", typeArgs.toList.map(s => List(Segment.Content(s))), "]")
      ) ++ Chain
        .fromSeq(argss)
        .map(args => Segment.simpleParameterBlock("(", args.toList.map(printExpr(_).toList), ")"))

    case AssignExpr(lhs, rhs) =>
      Segment.Content(lhs) +: (spaced("=") ++ printExpr(rhs))

    case NewExpr(extend, withs, members) =>
      Chain(
        Segment.Content("new"),
        Segment.Space,
        Segment.Content(extend)
      ) ++ printWiths(withs) :+ Segment.Space :+ Segment.simpleBlock(
        "{",
        printDefinitions(members).map(_.toList).toList,
        "}"
      ) :+ Segment.NewlineIfNotAlreadyPrinted

    case FreeformExpr(code) => Chain(Segment.Content(code))
  }
}
