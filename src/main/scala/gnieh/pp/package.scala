/*
* This file is part of the gnieh-pp project.
*
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

import scala.annotation.tailrec

import scala.collection.TraversableLike

import scala.language.implicitConversions

/** Pretty-printer library based on the Wadler's paper "A Prettier Printer" and Lindig's paper "Strictly Pretty". */
package object pp {

  sealed trait Mode
  case object Flat extends Mode
  case object Break extends Mode

  @tailrec
  protected[pp] def fits(width: Int, column: Int, docs: List[(Int, Mode, Doc)]): Boolean =
    if (width <= 0)
      false
    else docs match {
      case (i, m, EmptyDoc) :: docs        => fits(width, column, docs)
      case (i, m, ConsDoc(d1, d2)) :: docs => fits(width, column, (i, m, d1) :: (i, m, d2) :: docs)
      case (i, m, NestDoc(j, d)) :: docs   => fits(width, column, (i + j, m, d) :: docs)
      case (i, m, TextDoc(s)) :: docs      => fits(width - s.length, column, docs)
      case (i, Flat, LineDoc(s)) :: docs   => fits(width - s.length, column, docs)
      case (i, m, GroupDoc(d)) :: docs     => fits(width, column, (i, Break, d) :: docs)
      case (i, m, AlignDoc(d)) :: docs     => fits(width, column, (column, m, d) :: docs)
      case (i, m, ColumnDoc(f)) :: docs    => fits(width, column, (i, m, f(column)) :: docs)
      case _                               => true
    }

  /** Indents the document */
  @inline
  def nest(indent: Int)(inner: Doc): Doc =
    NestDoc(indent, inner)

  /** Deindent the document */
  @inline
  def unnest(indent: Int)(inner: Doc): Doc =
    nest(-indent)(inner)

  /** Renders as a space */
  @inline
  val space: Doc =
    TextDoc(" ")

  /** Renders as a new line unless it is discarded by a group, in which case behaves like `space` */
  @inline
  val line: Doc = LineDoc(" ")

  /** Renders as a new line unless it is discarded by a group, in which case behaves like `empty` */
  @inline
  val linebreak: Doc =
    LineDoc("")

  /** Behaves like `space` if the result fits in the page, otherwise behaves like `line` */
  @inline
  val softline: Doc =
    group(line)

  /** Behaves like `empty` if the result fits in the page, otherwise behaves like `line` */
  @inline
  val softbreak: Doc =
    group(linebreak)

  /** Behaves like a new line unless it is discarded by a group, in which case, behaves like the replacement document */
  @inline
  def lineOr(replacement: => String): Doc =
    LineDoc(replacement)

  /** Renders as an empty string */
  @inline
  val empty: Doc =
    EmptyDoc

  /** Renders the document with prepending n spaces to the current column */
  @inline
  def indent(i: Int)(doc: Doc): Doc =
    hang(i)(ConsDoc(TextDoc(" " * i), doc))

  /** Renders the document with nesting level set to the current column */
  @inline
  def align(doc: Doc): Doc =
    AlignDoc(doc)

  /** Renders the document with nesting level set to the current column plus `indent` */
  @inline
  def hang(indent: Int)(doc: Doc): Doc =
    align(nest(indent)(doc))

  /** Renders the text as is. If it contains new lines, `string` should be used. */
  def text(s: String): Doc =
    if (s.isEmpty)
      EmptyDoc
    else
      TextDoc(s)

  /** Concatenates all characters, using `line` for new lines and `text` for other blocks */
  def string(s: String): Doc =
    if (s.contains("\n"))
      s.split("\n").foldRight(empty) { (word, doc) =>
        text(word) :|: doc
      }
    else
      text(s)

  /** Splits the string into words and create a document for each word */
  def words(s: String): List[Doc] =
    s.split("\\s+").map(text).toList

  /** Creates a document from the given character */
  def char(c: Char): Doc =
    if (c == '\n')
      line
    else
      TextDoc(c.toString)

  /** Creates a document from the given integer */
  @inline
  def int(i: Int): Doc =
    TextDoc(i.toString)

  /** Creates a document from the given long */
  @inline
  def long(l: Long): Doc =
    TextDoc(l.toString)

  /** Creates a document from the given float */
  @inline
  def float(f: Float): Doc =
    TextDoc(f.toString)

  /** Creates a document from the given double */
  @inline
  def double(d: Double): Doc =
    TextDoc(d.toString)

  /** Discards all line breaks in the given document if the result fits in the page, otherwise, renders without any changes */
  @inline
  def group(doc: Doc): Doc =
    GroupDoc(doc)

  /** Renders the document as usual, and then fills until `width` with spaces if necessary */
  @inline
  def fill(until: Int)(doc: Doc): Doc =
    width { w =>
      if (w >= until)
        empty
      else
        text(" " * (until - w))
    }(doc)

  /** Renders a document followed by some other document computed depending on the current width */
  @inline
  def width(f: Int => Doc)(doc: Doc): Doc =
    ColumnDoc(start => doc :: ColumnDoc(end => f(end - start)))

  /** Renders a document in which all documents in the collection are appended horizontally, separated by a `space` */
  @inline
  def hsep(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :+: _)

  /** Renders a document in which all documents in the collection are appended vertically, separated by a `line` */
  @inline
  def vsep(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :|: _)

  /** Renders a document in which all documents in the collection are appended vertically, separated by a `softline` */
  @inline
  def fillSep(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :\: _)

  /** Renders a document that tries to append the documents in the collection horizontally separated by a  `space` if it fits, otherwise append them vertically */
  @inline
  def sep(docs: TraversableLike[Doc, _]): Doc =
    group(vsep(docs))

  /** Renders a document that appends the document in the collection horizontally */
  @inline
  def hcat(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :: _)

  /** Renders a document that appends all documents in the collection vertically, separated by a `linebreak` */
  @inline
  def vcat(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :||: _)

  /** Renders a document that appends all document in the collection horizontally, separated by a `softbreak` */
  @inline
  def fillCat(docs: TraversableLike[Doc, _]): Doc =
    docs.foldRight(empty)(_ :\\: _)

  /** Renders a document that trie to append the documents in the collection horizontally, otherwise append them vertically */
  @inline
  def cat(docs: TraversableLike[Doc, _]): Doc =
    group(vcat(docs))

  @inline
  implicit def s2doc(s: String): Doc =
    string(s)

  @inline
  implicit def i2doc(i: Int): Doc =
    int(i)

  @inline
  implicit def l2doc(l: Long): Doc =
    long(l)

  @inline
  implicit def f2doc(f: Float): Doc =
    float(f)

  @inline
  implicit def d2doc(d: Double): Doc =
    double(d)

  @inline
  implicit def c2doc(c: Char): Doc =
    char(c)

  implicit def opt2doc[T](o: Option[T])(implicit ev: T => Doc): Doc = o match {
    case Some(d) => d
    case None    => empty
  }

}
