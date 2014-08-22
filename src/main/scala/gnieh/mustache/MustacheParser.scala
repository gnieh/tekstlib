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
package gnieh.mustache

import org.parboiled2._

import shapeless._

import scala.language.implicitConversions

class MustacheParser(val input: ParserInput) extends Parser {

  import CharPredicate.{
    Alpha,
    AlphaNum
  }

  private var opening = "{{"
  private var closing = "}}"

  def MustacheFile: Rule1[List[Stmt]] = rule {
    Mustache ~ EOI
  }

  def Mustache: Rule1[List[Stmt]] = rule {
    zeroOrMore(Statement | Text) ~> ((s: Seq[Stmt]) => s.toList)
  }

  def Text: Rule1[Txt] = rule {
    capture(oneOrMore(!opening ~ ANY)) ~> ((txt: String) => Txt(txt))
  }

  def Statement: Rule1[Stmt] = rule {
    UnescapedVariable | Partial | Section | InvertedSection | Comment | SetDelimiter | EscapedVariable
  }

  def EscapedVariable: Rule1[Var] = rule {
    opening ~ Ident ~ closing ~> ((id: String) => Var(id, true))
  }

  def UnescapedVariable: Rule1[Var] = rule {
    opening ~ ("{" ~ Ident ~ "}" | "&" ~ Ident)~ closing ~> ((id: String) => Var(id, false))
  }

  def Section: Rule1[Sec] = rule {
    opening ~ "#" ~ Ident ~ closing ~ Mustache ~>
      ((name: String, content: List[Stmt]) =>
        closeTag(name, content)) ~> ((name: String, content: List[Stmt]) => Sec(name, content, false))
  }

  def InvertedSection: Rule1[Sec] = rule {
    opening ~ "^" ~ Ident ~ closing ~ Mustache ~>
      ((name: String, content: List[Stmt]) =>
        closeTag(name, content)) ~> ((name: String, content: List[Stmt]) => Sec(name, content, true))
  }

  def Partial: Rule1[Par] = rule {
    opening ~ ">" ~ WhiteSpace ~ capture(oneOrMore(!(WhiteSpaceChar | closing) ~ ANY)) ~ WhiteSpace ~ closing ~> ((id: String) => Par(id))
  }

  def Comment: Rule1[Stmt] = rule {
    opening ~ "!" ~ zeroOrMore(!closing ~ ANY) ~ closing ~> (() => Com)
  }

  def SetDelimiter: Rule1[Stmt] = rule {
    opening ~ "=" ~ WhiteSpace ~ capture(oneOrMore(!WhiteSpaceChar ~ ANY)) ~ oneOrMore(WhiteSpaceChar) ~ capture(oneOrMore(!(WhiteSpaceChar | '=')
      ~ ANY)) ~ "=" ~ closing ~> ((o: String, c: String) => {
        opening = o
        closing = c
        SetDelim
      })
  }

  def Ident: Rule1[String] = rule { WhiteSpace ~ capture(oneOrMore(Alpha | anyOf("_!?.-"))) ~ WhiteSpace }

  def WhiteSpace: Rule0 = rule { zeroOrMore(WhiteSpaceChar) }

  def WhiteSpaceChar: Rule0 = rule { anyOf(" \n\r\t\f") }

  def closeTag(name: String, content: List[Stmt]): Rule2[String, List[Stmt]] = rule {
    opening ~ '/' ~ Ident ~ closing ~> ((name2: String) => test(name == name2) ~> (() => name :: content :: HNil))
  }

}
