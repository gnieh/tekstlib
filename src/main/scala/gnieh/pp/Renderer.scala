/*
 * This file is part of the gnieh-pp project.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gnieh.pp

import scala.annotation.tailrec

/** A pretty printer, that tries to make the document fit in the page width
 *
 *  @author Lucas Satabin
 */
class PrettyRenderer(width: Int) extends (Doc => SimpleDoc) {

  private type Docs = List[(Int, Doc)]

  def apply(doc: Doc) =
    format(0, List((0, Break, doc)))

  private def format(column: Int, docs: List[(Int, Mode, Doc)]): SimpleDoc =
    docs match {
      case (i, m, EmptyDoc) :: docs        => format(column, docs)
      case (i, m, ConsDoc(d1, d2)) :: docs => format(column, (i, m, d1) :: (i, m, d2) :: docs)
      case (i, m, NestDoc(j, d)) :: docs   => format(column, (i + j, m, d) :: docs)
      case (i, m, TextDoc(s)) :: docs      => SText(s, format(column + s.length, docs))
      case (i, Flat, LineDoc(s)) :: docs   => SText(s, format(column + s.length, docs))
      case (i, Break, LineDoc(s)) :: docs  => SLine(i, format(i, docs))
      case (i, m, AlignDoc(d)) :: docs     => format(column, (column, m, d) :: docs)
      case (i, m, ColumnDoc(f)) :: docs    => format(column, (i, m, f(column)) :: docs)
      case (i, m, GroupDoc(d)) :: docs =>
        if (fits(width - column, column, (i, Flat, d) :: docs))
          format(column, (i, Flat, d) :: docs)
        else
          format(column, (i, Break, d) :: docs)
      case Nil => SEmpty
    }

}

/** This printer is not really pretty (but should be faster than pretty printers!):
 *  it does not insert any indentation and discards all groups, just renders everything as compact as possible
 *
 *  @author Lucas Satabin
 */
object CompactRenderer extends (Doc => SimpleDoc) {

  def apply(doc: Doc) =
    scan(0, List(doc))

  private def scan(column: Int, docs: List[Doc]): SimpleDoc = docs match {
    case Nil                     => SEmpty
    case EmptyDoc :: docs        => scan(column, docs)
    case TextDoc(s) :: docs      => SText(s, scan(column + s.length, docs))
    case LineDoc(s) :: docs      => SText(s, scan(column + s.length, docs))
    case ConsDoc(d1, d2) :: docs => scan(column, d1 :: d2 :: docs)
    case NestDoc(j, d) :: docs   => scan(column, d :: docs)
    case GroupDoc(d) :: docs     => scan(column, d :: docs)
    case AlignDoc(d) :: docs     => scan(column, d :: docs)
    case ColumnDoc(f) :: docs    => scan(column, f(column) :: docs)
  }

}

sealed trait CountUnit
/** Truncates after the count in non space characters */
case object Characters extends CountUnit
/** Truncates after the count in words */
case object Words extends CountUnit
/** Truncates after the count in lines */
case object Lines extends CountUnit

/** A renderer that truncates the result (once rendered by the inner renderer) with the given
 *  limit. It makes the assumption that the following invariants are respected:
 *   - a [[gnieh.pp.SText]] contains either only spaces or only a word
 *   - indentation characters are all modeled with the indentation in [[gnieh.pp.SLine]]
 *
 *  @author Lucas Satabin
 */
class TruncateRenderer(max: Int, unit: CountUnit, inner: Doc => SimpleDoc) extends (Doc => SimpleDoc) {

  def apply(doc: Doc) =
    truncate(inner(doc))

  def truncate(doc: SimpleDoc): SimpleDoc = {
    unit match {
      case Lines      => firstLines(max, doc)
      case Characters => firstChars(max, doc)
      case Words      => firstWords(max, doc)
    }
  }

  private def firstLines(maxLines: Int, doc: SimpleDoc): SimpleDoc = doc match {
    case SText(text, next) if maxLines >= 1  => SText(text, firstLines(maxLines, next))
    case SLine(indent, next) if maxLines > 1 => SLine(indent, firstLines(maxLines - 1, next))
    case _                                   => SEmpty
  }

  private def firstChars(maxChars: Int, doc: SimpleDoc): SimpleDoc = doc match {
    case SText(text, next) if maxChars >= text.replaceAll(" ", "").length => SText(text, firstChars(maxChars - text.length, next))
    case SLine(indent, next) if maxChars > 0 => SLine(indent, firstChars(maxChars, next))
    case _ => SEmpty
  }

  private def firstWords(maxWords: Int, doc: SimpleDoc): SimpleDoc = doc match {
    case SText(text, next) if text.trim.isEmpty && maxWords > 0 => SText(text, firstWords(maxWords, next))
    case SText(text, next) if maxWords > 0 => SText(text, firstWords(maxWords - 1, next))
    case SLine(indent, next) if maxWords > 0 => SLine(indent, firstWords(maxWords, next))
    case _ => SEmpty

  }

}
