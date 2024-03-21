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

import scala.annotation.tailrec

sealed trait CodePrinterSegment
object CodePrinterSegment {
  case class Content(str: String) extends CodePrinterSegment
  case class Block(
      open: List[CodePrinterSegment],
      content: List[List[CodePrinterSegment]],
      close: List[CodePrinterSegment]
  ) extends CodePrinterSegment
  case class ParameterBlock(
      onlyPrePostLineOnMultiline: Boolean,
      open: List[CodePrinterSegment],
      preline: List[CodePrinterSegment],
      content: List[List[CodePrinterSegment]],
      postline: List[CodePrinterSegment],
      close: List[CodePrinterSegment]
  ) extends CodePrinterSegment
  case object Newline                                                   extends CodePrinterSegment
  case object SpaceIfNotAlreadyPrinted                                  extends CodePrinterSegment
  case object NewlineIfNotAlreadyPrinted                                extends CodePrinterSegment
  case object Indent                                                    extends CodePrinterSegment
  case object Outdent                                                   extends CodePrinterSegment
  case class NewlineIfNotEnoughSpace(content: List[CodePrinterSegment]) extends CodePrinterSegment
  case object Space                                                     extends CodePrinterSegment

  def simpleBlock(open: String, content: List[List[CodePrinterSegment]], close: String): CodePrinterSegment.Block =
    Block(List(Content(open)), content, List(Content(close)))

  def simpleParameterBlock(
      open: String,
      content: List[List[CodePrinterSegment]],
      close: String
  ): CodePrinterSegment.ParameterBlock =
    ParameterBlock(false, List(Content(open)), Nil, content, List(Content(","), Space), List(Content(close)))
}

case class PrinterOptions(
    maxLineLength: Int
)

object CodePrinter {

  @tailrec
  def printRec(segments: List[CodePrinterSegment], indent: Int, acc: List[String])(
      implicit options: PrinterOptions
  ): List[String] = {
    import CodePrinterSegment._
    segments match {
      case Nil => acc
      case segment :: nextSegments =>
        def combineHead(str: String): String = acc.headOption.fold(str)(_ + str)

        segment match {
          case Content(str) => printRec(nextSegments, indent, combineHead(str) :: acc.drop(1))
          case Block(open, content, close) =>
            printRec(
              open ::: (Indent :: content.flatMap(c => Newline +: c))
                ::: (Outdent :: Newline :: close)
                ::: nextSegments,
              indent,
              acc
            )

          case ParameterBlock(onlyPrePostLineOnMultiline, open, preline, content, postline, close) =>
            lazy val contentWithExtra = content.init.map { c =>
              preline ::: c ::: postline
            } :+ (preline ::: content.last)

            lazy val singlelineContent = if (onlyPrePostLineOnMultiline) content.flatten else contentWithExtra.flatten

            lazy val printContent = print(open ::: singlelineContent ::: close)

            printRec(
              if (content.isEmpty)
                nextSegments
              else if (printContent.length > 1 || combineHead(printContent.mkString("")).length > options.maxLineLength)
                Block(open, contentWithExtra, close) :: nextSegments
              else
                open ::: singlelineContent ::: close ::: nextSegments,
              indent,
              acc
            )

          case SpaceIfNotAlreadyPrinted =>
            printRec(if (combineHead("").lastOption.contains(' ')) nextSegments else Space :: nextSegments, indent, acc)

          case NewlineIfNotAlreadyPrinted =>
            val justNewline = combineHead("") == "  " * indent
            printRec(if (justNewline) nextSegments else Newline :: nextSegments, indent, acc)

          case Newline =>
            printRec(nextSegments, indent, ("  " * indent) :: acc)

          case Indent  => printRec(nextSegments, indent + 1, acc)
          case Outdent => printRec(nextSegments, indent - 1, acc)
          case NewlineIfNotEnoughSpace(content) =>
            val printContent = print(content)
            val isBlocks = content.forall {
              case _: Block          => true
              case _: ParameterBlock => true
              case _                 => false
            }

            printRec(
              if (!isBlocks && combineHead(printContent.mkString("")).length > options.maxLineLength)
                (Indent :: Newline :: content) ::: (Outdent :: nextSegments)
              else content ::: nextSegments,
              indent,
              acc
            )

          case Space =>
            printRec(nextSegments, indent, combineHead(" ") :: acc.drop(1))
        }
    }
  }

  def print(segments: List[CodePrinterSegment], indent: Int = 0)(implicit options: PrinterOptions): List[String] =
    // segments.map(_.toString)
    printRec(segments, indent, Nil).reverse
}
