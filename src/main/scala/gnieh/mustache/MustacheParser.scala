/*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh
package mustache

import string.StringScanner

import regex._
import bytecode._

import scala.util.{
  Try,
  Success,
  Failure
}

import scala.annotation.tailrec

class MustacheParser(val input: String) {

  private type SectionStack = List[(String, Boolean, List[Statement])]

  private val scanner = new StringScanner(input)

  private val validTypesRe =
    "#^/=!<>&{".map(Regex.escape(_)).mkString("(", "|", ")?").re

  private val skipWhitespace =
    Set("#", "^", "/", "<", ">", "=", "!")

  private val allowedContent =
    """(\w|[?!\\/.-])*""".re

  private val whiteSpaceRe = "[ \t]*".re

  private val eolRe = "[ \t]*\r?\n".re

  private val anyContent = Set("!", "=")

  private var openTag = "{{"

  private var closeTag = "}}"

  private var openRe = Regex.escape(openTag).re

  private var paddedOpenRe = f"[ \t]*${Regex.escape(openTag)}".re

  private def setOpen(o: String) = {
    openTag = o
    openRe = Regex.escape(openTag).re
    paddedOpenRe = f"[ \t]*${Regex.escape(openTag)}".re
  }

  private def setClose(c: String) =
    closeTag = c

  private var padding = ""

  def run(): Try[List[Statement]] =
    parseStatements(List(("", false, Nil)))

  @tailrec
  final def parseStatements(sections: SectionStack): Try[List[Statement]] =
    parseStatement(sections) match {
      case Success(List(("", false, stmts))) if scanner.eos =>
        Success(stmts.reverse)
      case Success((name, _, _) :: _) if scanner.eos =>
        Failure(new MustacheException(f"Unclosed section $name"))
      case Success(Nil) =>
        Failure(new MustacheException("Closed too many sections"))
      case Success(sections) =>
        parseStatements(sections)
      case Failure(f) =>
        Failure(f)
    }

  def parseStatement(sections: SectionStack): Try[SectionStack] =
    if (scanner.matches(paddedOpenRe))
      parseTag(sections)
    else
      parseText(sections)

  def parseTag(sections: SectionStack): Try[SectionStack] = {
    // remember if this is the beginning of line
    val bol = scanner.bol
    // remember the starting position
    val startPos = scanner.pointer
    // the padding are all the withespaces before the opening of a tag
    val padding = scanner.scan(whiteSpaceRe)

    sections match {
      case (name, inverted, stmts) :: rest =>

        scanner.scan(openRe) match {
          case Some(_) =>

            val stmts1 = padding match {
              case Some(p) if p.nonEmpty && !bol =>
                // this is not the beginning of line, we can push back the padding
                // as standard text if any
                Text(p) :: stmts
              case _ =>
                stmts
            }

            // get the tag type
            scanner.scan(validTypesRe) match {
              case Some(tpe) =>

                // the closing type is usually empty, except for the mustache notation and set delimiter
                val ctpe = if (tpe == "{") "}" else if (tpe == "=") "=" else ""

                // skip the potential whitespaces
                scanner.skip(whiteSpaceRe)

                val closingRe = f"\\s*$ctpe$closeTag".re

                val content = if (anyContent.contains(tpe)) {
                  // any content is allowed here, we scan until the closing tag
                  Some(scanner.scanUntilExclusive(closingRe))
                } else {
                  // otherwise we get only standard allowed characters
                  scanner.scan(allowedContent)
                }

                // eat the end of the tag
                if (scanner.skip(closingRe) == 0) {
                  Failure(new MustacheException("Unclosed tag"))
                } else {

                  // clean whitespaces when needed i.e. the tag is the only non whitespace
                  // character of the line
                  val stmts2 = if (bol && skipWhitespace.contains(tpe) && scanner.matches(eolRe)) {
                    scanner.skip(eolRe)
                    stmts1
                  } else if (bol) {
                    padding match {
                      case Some(p) if p.nonEmpty =>
                        Text(p) :: stmts1
                      case _ =>
                        stmts1
                    }
                  } else {
                    stmts1
                  }

                  content match {
                    case Some(c) =>
                      tpe match {
                        case "" =>
                          // no tag type => escaped variable
                          Success((name, inverted, Variable(c, true) :: stmts2) :: rest)
                        case "&" | "{" =>
                          // unescaped variable
                          Success((name, inverted, Variable(c, false) :: stmts2) :: rest)
                        case "!" =>
                          // comment, just ignore it
                          Success((name, inverted, stmts2) :: rest)
                        case ">" | "<" =>
                          // partial inclusion
                          Success((name, inverted, Partial(c) :: stmts2) :: rest)
                        case "=" =>
                          // change delimiters
                          c.split(" ", 2) match {
                            case Array(o, c) =>
                              setOpen(o)
                              setClose(c)
                              Success((name, inverted, stmts2) :: rest)
                            case _ =>
                              Failure(new MustacheException("Malformed set-delimiter tag"))
                          }
                        case "#" =>
                          // opening section
                          parseStatement((c, false, Nil) :: (name, inverted, stmts2) :: rest)
                        case "^" =>
                          // opening inverted section
                          parseStatement((c, true, Nil) :: (name, inverted, stmts2) :: rest)
                        case "/" =>
                          if (name == c) {
                            rest match {
                              case (lastName, lastInverted, lastStmts) :: rest1 =>
                                Success(
                                  (lastName, lastInverted,
                                    Section(name, stmts2.reverse, inverted) :: lastStmts) :: rest1)
                              case Nil =>
                                Failure(new MustacheException("Closing unopen section"))
                            }
                          } else {
                            Failure(new MustacheException(f"Closing section $c but expected closing section $name"))
                          }
                      }
                    case None =>
                      Failure(new MustacheException("Illegal content in tag"))
                  }
                }
              case None =>
                Failure(new MustacheException(f"Unknown tag type ${scanner.peek(1)}"))
            }
          case None =>
            Failure(new MustacheException("A tag was expected"))
        }

      case Nil =>
        Failure(new MustacheException("Content after the end of input???"))
    }
  }

  def parseText(sections: SectionStack): Try[SectionStack] =
    sections match {
      case (name, inverted, stmts) :: rest =>
        val txt = scanner.scanUntilExclusive(paddedOpenRe)
        if (txt.nonEmpty)
          Success((name, inverted, Text(txt) :: stmts) :: rest)
        else
          Success(sections)
      case Nil =>
        Failure(new MustacheException("Text after the end of input???"))
    }

}
