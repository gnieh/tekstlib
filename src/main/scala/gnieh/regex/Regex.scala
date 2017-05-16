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
package gnieh.regex

import compiler._
import util._

import scala.util.Failure

/** This class provides a way to create and use regular expressions. The actual implementation depends on the imported backend.
 *  By default we provide a non backtracking implementation [[gnieh.regex.vm.BytecodeImpl]] and a TDFA implementation [[gnieh.regex.tdfa.TDfaImpl]]. See the documentation of each class for details about the supported features.
 *
 *  @author Lucas Satabin
 */
class Regex(re: Either[ReNode, String], impl: RegexImpl) extends Serializable {

  def this(source: String, impl: RegexImpl) =
    this(Right(source), impl)

  def this(re: ReNode, impl: RegexImpl) =
    this(Left(re), impl)

  private val (saved, compiled) = re match {
    case Left(re)      => impl.compile(re)
    case Right(source) => impl.compile(Parser.parse(source).get)
  }

  /** Tells whether this regular expression is matched by the given input */
  def isMatchedBy(input: String): Boolean =
    impl.exec(compiled, saved, 0, input) match {
      case (-1, -1, _) =>
        false
      case (start, end, _) =>
        start == 0 && end == input.length
    }

  /** Finds the first match of this regular expression in the input.
   *  If nothing matches, returns `None`
   */
  def findFirstIn(input: String): Option[String] =
    for {
      m <- findFirstMatchIn(input)
      matched <- m.matched
    } yield matched

  /** Finds the first match of this regular expression in the input.
   *  If nothing matches, returns `None`
   */
  def findFirstMatchIn(input: String): Option[Match] = {
    def find(startIdx: Int): Option[Match] =
      impl.exec(compiled, saved, startIdx, input) match {
        case (-1, -1, _) if startIdx < input.size =>
          find(startIdx + 1)
        case (-1, -1, _) =>
          None
        case (start, end, groups) =>
          Some(new Match(start, end, groups, input))
      }
    find(0)
  }

  /** Finds all matches of this regular expression in the input. */
  def findAllIn(input: String): Iterator[String] =
    for {
      m <- findAllMatchIn(input)
      matched <- m.matched
    } yield matched

  /** Finds all matches of this regular expression in the input. */
  def findAllMatchIn(input: String): Iterator[Match] = {
    def loop(startIdx: Int): Stream[Match] =
      impl.exec(compiled, saved, startIdx, input) match {
        case (-1, -1, _) if startIdx < input.size =>
          loop(startIdx + 1)
        case (-1, -1, _) =>
          Stream.empty
        case (start, end, groups) =>
          val m = new Match(start, end, groups, input)
          if (start == end && end == input.size)
            // this is an empty match and we reach the end of the input
            // just return this match and stop
            Stream(m)
          else
            m #:: loop(end)
      }
    loop(0).iterator
  }

  def unapplySeq(input: String): Option[List[String]] =
    for {
      m @ Match(start, end) <- findFirstMatchIn(input)
      if start == 0 && end == input.size
    } yield m.subgroups

  override def toString =
    re match {
      case Left(re)  => re.toString
      case Right(re) => re
    }

}

object Regex {

  def apply(str: String)(implicit impl: RegexImpl): Regex =
    new Regex(str, impl)

  /** Escaped version of this character if it is needed. */
  def escape(c: Char): String =
    if (".[{()\\*+?|".contains(c))
      f"\\$c"
    else
      c.toString

  /** Escaped version of this string if it is needed. */
  def escape(s: String): String =
    s.map(escape(_)).mkString

}

