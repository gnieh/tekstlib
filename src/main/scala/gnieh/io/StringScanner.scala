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
package io

import regex.{
  Regex,
  Match
}

/** A string scanner, modeled after the ruby `StringScanner` class.
 *
 *  @author Lucas Satabin
 */
class StringScanner(private var _source: String) {

  // curent scan pointer
  private var _pointer = 0

  // indicates wether the pointer is at the beginning of a line
  private var _bol = true

  // returns the last matched string if any
  private var _lastMatched: Option[Match] = None

  /** Returns the current pointer in the scanner. */
  def pointer: Int =
    _pointer

  /** Sets the current pointer at the given position.
   *  Reset the `lastMatched` data. */
  def pointer_=(pos: Int): Unit = {
    if(pos < 0)
      _pointer = 0
    else if(pos > _source.size)
      _pointer = _source.size
    else
      _pointer = pos
    _bol = _pointer == 0 || _source(pointer - 1) == '\n'
  }

  /** Returns the last matched string if any, or `None` otherwise. */
  def lastMatched: Option[Match] =
    _lastMatched

  /** Returns the next character, or `None` if EOS was reached. */
  def get(): Option[Char] =
    if(_pointer == _source.size) {
      None
    } else {
      // advance the _pointer
      val res = _source(_pointer)
      _lastMatched = Some(new Match(0, 1, Vector(), res.toString))
      _bol = res == '\n'
      _pointer += 1
      // return the character
      Some(res)
    }

  /** Tries to match the given regular expression and returns the matched result, advancing
   *  the _pointer by the size of the matching substring.
   *  If it did not match, the pointer is left unchanged, and `None` is returned. */
  def scan(re: Regex): Option[String] =
    check(re) match {
      case Some(s) =>
        _pointer += s.size
        _bol = _pointer == 0 || (if(s.nonEmpty) s.last == '\n' else _bol)
        Some(s)
      case None =>
        None
    }

  /** Scans the string until the pattern is matched and consumed.
   *  Returns the substring up to and including the end of the match, advancing the
   *  pointer by the size of the matching substring.
   *  If it did not match, the pointer is left unchanged, and `None` is returned. */
  def scanUntil(re: Regex): Option[String] =
    checkUntil(re) match {
      case Some(s) =>
        _pointer += s.size
        _bol = _pointer == 0 || (if(s.nonEmpty) s.last == '\n' else _bol)
        Some(s)
      case None =>
        None
    }

  /** Scans the string until the pattern is matched but not consumed.
   *  Returns the substring up to and excluding the start of the match, advancing the
   *  pointer by the size of the matching substring.
   *  If it did not match, the pointer is left unchanged, and `None` is returned. */
  def scanUntilExclusive(re: Regex): String = {
    val s = checkUntilExclusive(re)
    _pointer += s.size
    _bol = _pointer == 0 || (if(s.nonEmpty) s.last == '\n' else _bol)
    s
  }

  /** Tries to skip the given regular expression starting at the current _pointer.
   *  Advances the pointer by the size of the matching string, and returns this size.
   *  If it did not match, `0` is returned and the pointer is not advanced. */
  def skip(re: Regex): Int =
    scan(re) match {
      case Some(s) =>
        s.size
      case None =>
        0
    }

  /** Advances the pointer until pattern is matched and consumed.
   *  Returns the size of consumed string. */
  def skipUntil(re: Regex): Int =
    scanUntil(re) match {
      case Some(s) =>
        s.size
      case None =>
        0
    }

  /** Advances the pointer until pattern is matched but not consumed.
   *  Returns the size of consumed string. */
  def skipUntilExclusive(re: Regex): Int =
    scanUntilExclusive(re).size

  /** Returns the same result as `scan` would return without advancing the pointer.
   *  The `lastMatched` however is updated. */
  def check(re: Regex): Option[String] =
    re.findFirstMatchIn(_source.substring(_pointer)) match {
      case Some(m @ Match(0, _)) =>
        // advance the _pointer by the match length
        _lastMatched = Some(m)
        m.matched
      case _ =>
        // it either did not match or matched in the middle
        None
    }

  /** Returns the same result as `scanUntil` would return without advancing the pointer.
   *  The `lastMatched` however is updated. */
  def checkUntil(re: Regex): Option[String] =
    re.findFirstMatchIn(_source.substring(_pointer)) match {
      case Some(m) =>
        // a match was found, returns everything up to the end of the match
        // and advance the _pointer
        _lastMatched = Some(m)
        Some(_source.substring(_pointer, _pointer + m.end))
      case None =>
        None
    }

  /** Returns the same result as `scanUntilExclusive` would return without advancing the pointer.
   *  The `lastMatched` however is updated. */
  def checkUntilExclusive(re: Regex): String = {
    val rest = _source.substring(_pointer)
    re.findFirstMatchIn(rest) match {
      case Some(m) =>
        // a match was found, returns everything up to the end of the match
        // and advance the _pointer
        _lastMatched = Some(m)
        _source.substring(_pointer, _pointer + m.start)
      case None =>
        rest
    }
  }

  /** Checks that the string amtches the regular expression at the current pointer.
   *  Neither the `lastMatched` nor the `pointer` are updated. */
  def matches(re: Regex): Boolean =
    re.findFirstMatchIn(_source.substring(_pointer)) match {
      case Some(Match(0, _)) => true
      case _                 => false
    }

  /** Extracts the string starting at the current pointer of the given length.
   *  If fewer characters are left in the string, then the maximum possible
   *  number of characters is returned. */
  def peek(length: Int): String =
    _source.substring(_pointer, math.min(_source.size - _pointer, length))

  /** Indicates whether the current pointer is at the beginning of a line. */
  def bol(): Boolean =
    _bol

  /** Indicates whether the current pointer is at the end of the source string. */
  def eos(): Boolean =
    _pointer == _source.size

  /** Returns the (possibly empty) rest string starting at the current pointer. */
  def rest(): String =
    if(eos)
      ""
    else
      _source.substring(_pointer)

  /** Reset the pointer (at position 0) and clears the last match. */
  def reset(): Unit = {
    _pointer = 0
    _lastMatched = None
    _bol = true
  }

  /** Set the pointer at the end of the input and clears the last match. */
  def terminate(): Unit = {
    _pointer = _source.size
    _lastMatched = None
    _bol = _source.last == '\n'
  }

  /** Appends the given string to the current source. It does not affect the pointer. */
  def append(str: String): Unit =
    _source += str

  /** Returns the string being scanned */
  def source: String =
    _source

  /** Sets the string to scan and reset this scanner. */
  def source_=(str: String): Unit = {
    _source = str
    reset()
  }

  /** Returns the i-th captured group of the last match if any. */
  def apply(i: Int): Option[String] =
    _lastMatched.flatMap(_.group(i))

}
