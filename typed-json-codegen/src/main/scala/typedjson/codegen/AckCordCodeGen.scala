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

import io.circe.*

/*
object AckCordCodeGen extends TypedJsonCodeGen {
  object codeGenTypes extends CodeGenTypes {
    case class RequestDef(
        name: String,
        imports: Seq[String],
        documentation: Option[String],
        path: Seq[PathElem],
        method: String,
        query: Option[AnonymousClassTypeDef],
        arrayOfBody: Boolean,
        body: Option[AnonymousClassTypeDefOrType],
        arrayOfReturn: Boolean,
        returnTpe: Option[AnonymousClassTypeDefOrType],
        allowsReason: Boolean,
        additionalTypeParams: Seq[String],
        additionalParams: Map[String, RequestDefAdditionalParam],
        complexType: RequestDefComplexType,
        encodeBody: Option[String],
        parseResponse: Option[String]
    ) extends TypeDef.OtherTypeDef

    override protected def decodeOtherTypeDef(other: String, c: HCursor): Decoder.Result[TypeDef.OtherTypeDef] =
      other match {
        case "Request" =>
          for {
            imports              <- c.getOrElse[Seq[String]]("imports")(Nil)
            documentation        <- c.get[Option[String]]("documentation")
            name                 <- c.get[String]("name")
            path                 <- c.get[Seq[PathElem]]("path")
            method               <- c.get[String]("method")
            query                <- c.get[Option[AnonymousClassTypeDef]]("query")
            arrayOfBody          <- c.getOrElse[Boolean]("arrayOfBody")(false)
            body                 <- c.get[Option[AnonymousClassTypeDefOrType]]("body")
            arrayOfReturn        <- c.getOrElse[Boolean]("arrayOfReturn")(false)
            returnTpe            <- c.get[Option[AnonymousClassTypeDefOrType]]("return")
            allowsReason         <- c.getOrElse[Boolean]("allowsReason")(false)
            additionalTypeParams <- c.getOrElse[Seq[String]]("additionalTypeParams")(Nil)
            additionalParams     <- c.getOrElse[Map[String, RequestDefAdditionalParam]]("additionalParams")(Map.empty)
            complexType   <- c.getOrElse[RequestDefComplexType]("complexType")(RequestDefComplexType("Any", "Any"))
            encodeBody    <- c.get[Option[String]]("encodeBody")
            parseResponse <- c.get[Option[String]]("parseResponse")
          } yield RequestDef(
            name,
            imports,
            documentation,
            path,
            method,
            query,
            arrayOfBody,
            body,
            arrayOfReturn,
            returnTpe,
            allowsReason,
            additionalTypeParams,
            additionalParams,
            complexType,
            encodeBody,
            parseResponse
          )
      }

    sealed trait PathElem

    object PathElem {
      case class StringPathElem(elem: String) extends PathElem

      case class ArgPathElem(name: Option[String], argOf: String, major: Boolean, documentation: Option[String])
          extends PathElem

      case class CustomArgPathElem(
          name: String,
          tpe: String,
          major: Boolean,
          documentation: Option[String]
      ) extends PathElem

      implicit lazy val pathElemDecoder: Decoder[PathElem] = (c: HCursor) =>
        c.as[String]
          .map[PathElem](StringPathElem(_))
          .swap
          .map[Either[DecodingFailure, PathElem]] { _ =>
            for {
              name          <- c.get[Option[String]]("name")
              argOf         <- c.get[String]("argOf")
              major         <- c.getOrElse[Boolean]("major")(false)
              documentation <- c.get[Option[String]]("documentation")
            } yield ArgPathElem(name, argOf, major, documentation)
          }
          .swap
          .joinLeft
          .swap
          .map { _ =>
            for {
              name          <- c.get[String]("name")
              tpe           <- c.get[String]("customArgType")
              major         <- c.getOrElse[Boolean]("major")(false)
              documentation <- c.get[Option[String]]("documentation")
            } yield CustomArgPathElem(name, tpe, major, documentation)
          }
          .swap
          .joinLeft
    }

    case class RequestDefAdditionalParam(tpe: String, default: Option[String])

    object RequestDefAdditionalParam {
      implicit lazy val requestDefAdditionalParamsDecoder: Decoder[RequestDefAdditionalParam] = (c: HCursor) =>
        c.as[String]
          .map(RequestDefAdditionalParam(_, None))
          .swap
          .map[Either[DecodingFailure, RequestDefAdditionalParam]] { _ =>
            for {
              tpe     <- c.get[String]("type")
              default <- c.get[Option[String]]("default")
            } yield RequestDefAdditionalParam(tpe, default)
          }
          .swap
          .joinLeft
    }

    case class RequestDefComplexType(r1: String, r2: String) {
      def isEmpty: Boolean = r1 == "Any" && r2 == "Any"
    }

    object RequestDefComplexType {
      implicit lazy val requestDefComplexTypeDecoder: Decoder[RequestDefComplexType] = (c: HCursor) =>
        for {
          r1 <- c.getOrElse[String]("R1")("Any")
          r2 <- c.getOrElse[String]("R2")("Any")
        } yield RequestDefComplexType(r1, r2)
    }
  }
  import codeGenTypes.*

  override protected def automaticImportsOtherTypedef(typeDef: TypeDef.OtherTypeDef): Seq[String] = typeDef match {
    case td: RequestDef =>
      Seq("ackcord.requests._", "sttp.model.Method") ++ td.query.toSeq.flatMap(anon =>
        automaticImports(anon.named(""))
      ) ++ td.body.toSeq.flatMap {
        case AnonymousClassTypeDefOrType.TypeRef(_)     => Nil
        case AnonymousClassTypeDefOrType.AnonType(anon) => automaticImports(anon.named(""))
      } ++ td.returnTpe.toSeq.flatMap {
        case AnonymousClassTypeDefOrType.TypeRef(_)     => Nil
        case AnonymousClassTypeDefOrType.AnonType(anon) => automaticImports(anon.named(""))
      }
    case _ => throw new Exception(s"Unknown typedef $typeDef")
  }

  override protected def codeFromOtherTypeDef(typeDef: TypeDef.OtherTypeDef): List[GenAST.Definition] = typeDef match {
    case requestDef: RequestDef => codeFromRequestDef(requestDef)
    case _                      => throw new Exception(s"Unknown typedef $typeDef")
  }

  def knownArgPathElemToCustom(elem: PathElem.ArgPathElem): PathElem.CustomArgPathElem = {
    def custom(tpe: String, name: Option[String] = None, majorParameter: Boolean = false) =
      PathElem.CustomArgPathElem(
        elem.name.orElse(name).getOrElse(tpe.charAt(0).toLower.toString + tpe.substring(1)),
        tpe,
        majorParameter,
        elem.documentation
      )

    val allowedNormal = Set(
      "GuildId",
      "ChannelId",
      "ApplicationId",
      "CommandId",
      "EmojiId",
      "MessageId",
      "UserId",
      "Emoji",
      "RoleId",
      "WebhookId",
      "GuildScheduledEventId",
      "InteractionId"
    )

    elem.argOf match {
      case "webhookToken"                 => custom("String", Some("webhookToken"))
      case "interactionToken"             => custom("String", Some("interactionToken"))
      case s if allowedNormal.contains(s) => custom(s)
      case _ =>
        sys.error(s"Unknown path arg element ${elem.argOf}")
    }
  }

  def codeFromRequestDef(requestDef: RequestDef): List[GenAST.Definition] = {
    val rhs = {
      val pathArg = requestDef.path
        .foldLeft(("", true)) { case ((acc, firstArg), arg) =>
          def handleCustom(custom: PathElem.CustomArgPathElem): (String, Boolean) = {
            val name       = custom.name
            val majorTypes = Set("GuildId", "ChannelId", "WebhookId")
            val major =
              if (custom.major || (majorTypes.contains(custom.tpe) && firstArg)) ", major = true" else ""
            (s"""$acc / Parameters[${custom.tpe}]("$name", $name$major)""", false)
          }

          arg match {
            case PathElem.StringPathElem(elem)      => (s"""$acc / "$elem"""", firstArg)
            case arg: PathElem.ArgPathElem          => handleCustom(knownArgPathElemToCustom(arg))
            case custom: PathElem.CustomArgPathElem => handleCustom(custom)
          }
        }
        ._1

      val queryArg = requestDef.query.filter(_.fields.nonEmpty).fold("") { q =>
        val highestVersion = q.fields.keys.maxBy(_.replace(".", "").replace("x", "").toInt)
        q
          .fields(highestVersion)
          .map { case (k, v) =>
            val queryParam =
              if (isFieldUndefined(v, q.allUndefined)) s"""Parameters.query("$k", query.${camelCaseFromSnakecase(k)})"""
              else s"""Parameters.queryAlways("$k", query.${camelCaseFromSnakecase(k)})"""

            s" +? $queryParam"
          }
          .mkString
      }

      val pathArgWithQuery = s"Route.Empty$pathArg$queryArg"

      GenAST.FunctionCall(
        s"Request.${if (requestDef.complexType.isEmpty) "restRequest" else "complexRestRequest"}",
        Nil,
        Seq(
          Seq(
            Some(
              GenAST.AssignExpr(
                "route",
                GenAST.FunctionCall(
                  s"($pathArgWithQuery).toRequest",
                  Nil,
                  Seq(Seq(GenAST.FreeformExpr(s"Method.${requestDef.method}")))
                )
              )
            ),
            Option.when(requestDef.body.nonEmpty)(GenAST.AssignExpr("params", GenAST.FreeformExpr("body"))),
            Option.when(requestDef.allowsReason)(
              GenAST.AssignExpr(
                "extraHeaders",
                GenAST.FreeformExpr("""reason.fold(Map.empty[String, String])(r => Map("X-Audit-Log-Reason" -> r))""")
              )
            ),
            requestDef.encodeBody.map(e => GenAST.AssignExpr("requestBody", GenAST.FreeformExpr(s"Some($e)"))),
            requestDef.parseResponse.map(r => GenAST.AssignExpr("parseResponse", GenAST.FreeformExpr(s"Some($r)")))
          ).flatten
        )
      )
    }

    val uncapitalizedName = requestDef.name.charAt(0).toLower.toString + requestDef.name.substring(1)
    val capitalizedName   = uncapitalizedName.capitalize

    val allCustomPathElems = requestDef.path.map {
      case argPathElem: PathElem.ArgPathElem => knownArgPathElemToCustom(argPathElem)
      case other                             => other
    }

    val pathParamNames = allCustomPathElems.collect { case PathElem.CustomArgPathElem(name, _, _, _) =>
      name
    }

    val duplicatePathParamNames = pathParamNames.collect {
      case name if pathParamNames.count(_ == name) > 1 => name
    }
    require(
      duplicatePathParamNames.isEmpty,
      s"Found duplicated name for request ${requestDef.name}. Duplicated: ${duplicatePathParamNames.mkString(", ")}"
    )

    val returnTpe = {
      val tpe = requestDef.returnTpe.fold("Unit") {
        case AnonymousClassTypeDefOrType.TypeRef(name)  => name
        case AnonymousClassTypeDefOrType.AnonType(anon) => s"${capitalizedName}Result"
      }
      if (requestDef.arrayOfReturn) s"Seq[$tpe]" else tpe
    }

    val typeParams = requestDef.additionalTypeParams.map(s => GenAST.TypeParameter(name = s))

    val paramsType = {
      val tpe = requestDef.body.fold("Unit") {
        case AnonymousClassTypeDefOrType.TypeRef(name) => name
        case AnonymousClassTypeDefOrType.AnonType(_)   => s"${capitalizedName}Body"
      }
      if (requestDef.arrayOfBody) s"Seq[$tpe]" else tpe
    }

    val requestType =
      if (requestDef.complexType.isEmpty) s"Request[$paramsType, $returnTpe]"
      else s"ComplexRequest[$paramsType, $returnTpe, ${requestDef.complexType.r1}, ${requestDef.complexType.r2}]"

    val params = {
      val pathParams = allCustomPathElems.collect { case PathElem.CustomArgPathElem(name, tpe, _, documentation) =>
        GenAST.Parameter(docs = documentation, name = name, tpe = tpe)
      }

      val queryParam = requestDef.query.map { query =>
        val highestVersion = query.fields.keys.map(_.replace(".", "").replace("x", "").toInt).max
        GenAST.Parameter(
          name = "query",
          tpe = s"${capitalizedName}Query",
          default =
            if (query.allUndefined)
              Some(GenAST.FunctionCall(s"${capitalizedName}Query.make$highestVersion", Nil, Seq(Nil)))
            else None
        )
      }.toList

      val bodyParam = requestDef.body.map(_ => GenAST.Parameter(name = "body", tpe = paramsType)).toList
      val reasonParam =
        if (requestDef.allowsReason) List(GenAST.Parameter(name = "reason", tpe = "Option[String]")) else Nil
      val additionalParams =
        requestDef.additionalParams.map { t =>
          GenAST.Parameter(name = t._1, tpe = t._2.tpe, default = t._2.default.map(GenAST.FreeformExpr(_)))
        }

      pathParams ++ queryParam ++ bodyParam ++ reasonParam ++ additionalParams
    }

    val hasParams     = params.nonEmpty
    val hasTypeParams = typeParams.nonEmpty

    val requestDefDef =
      if (!hasTypeParams && !hasParams)
        GenAST.ValDef(
          docs = requestDef.documentation,
          name = uncapitalizedName,
          returnType = requestType,
          rhs = Some(rhs)
        )
      else
        GenAST.DefDef(
          docs = requestDef.documentation,
          name = uncapitalizedName,
          typeParameters = typeParams,
          parameters = Seq(params),
          returnType = requestType,
          rhs = Some(rhs)
        )

    val queryClass = requestDef.query.map(q => codeFromClassTypeDef(q.named(capitalizedName + "Query")))
    val bodyClass = requestDef.body.collect { case AnonymousClassTypeDefOrType.AnonType(anon) =>
      codeFromClassTypeDef(anon.named(capitalizedName + "Body"))
    }
    val returnClass = requestDef.returnTpe.collect { case AnonymousClassTypeDefOrType.AnonType(anon) =>
      codeFromClassTypeDef(anon.named(capitalizedName + "Result"))
    }

    List(queryClass, bodyClass, returnClass).flatten :+ requestDefDef
  }
}
*/
