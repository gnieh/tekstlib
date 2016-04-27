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
package string

import scala.annotation.tailrec

/** An implementation of a gap buffer.
 *
 *  @author Lucas Satabin
 */
class GapBuffer(initialSize: Int) {

  // the buffer itself
  private var _buffer = Array.ofDim[Char](initialSize)

  // the current gap start index (included)
  private var _gapStart = 0

  // the current buffer size
  private def _size = _buffer.size

  // the current gap end index (excluded)
  private var _gapEnd = _size

  // the size of the current gap
  private def gapSize =
    _gapEnd - _gapStart

  /** Returns the index of the current cursor position. */
  def cursor: Int =
    _gapStart

  /** Sets the cursor at the given position. */
  def cursor_=(pos: Int): Unit =
    moveBy(pos - cursor)

  /** Returns the size of the buffer. */
  def bufferSize: Int =
    _size

  /** Returns the size of the buffer content. */
  def contentSize: Int =
    _size - (_gapEnd - _gapStart)

  /** Moves the cursor by the given amount of characters.
   *  A positive `delta` moves the cursor to the right, a negative one moves
   *  it to the left.
   */
  def moveBy(delta: Int): Unit =
    if (_gapStart + delta > contentSize || _gapStart + delta < 0) {
      // check delta bounds
      throw new Exception(f"Cursor index out of buffer: ${cursor + delta}")
    } else if (delta != 0) {
      val oldStart = _gapStart
      val oldEnd = _gapEnd
      // compute the new gap position and size
      _gapStart += delta
      _gapEnd = math.min(_gapEnd + delta, _size)
      if (delta > 0) {
        // moved to the right
        System.arraycopy(_buffer, oldEnd, _buffer, oldStart, delta)
      } else {
        // moved to the left
        System.arraycopy(_buffer, oldStart + delta, _buffer, oldEnd + delta, -delta)
      }
      regap()
    }

  /** Insert the given character at the current cursor position */
  def insert(c: Char): Unit = {
    _buffer(_gapStart) = c
    _gapStart += 1
    regap()
  }

  /** Inserts the given string at the current cursor position */
  def insert(s: String): Unit = {
    @tailrec
    def loop(s: String): Unit =
      if (s.size <= gapSize) {
        s.getChars(0, s.size, _buffer, _gapStart)
        _gapStart += s.size
        regap()
      } else {
        val size = gapSize
        s.getChars(0, size, _buffer, _gapStart)
        _gapStart += size
        regap()
        loop(s.substring(size))
      }
    loop(s)
  }

  /** Returns the content of this buffer as a string. */
  override def toString: String =
    if (gapSize == 0) {
      new String(_buffer)
    } else {
      val start = new String(_buffer, 0, _gapStart)
      val end = new String(_buffer, _gapEnd, _size - _gapEnd)
      start + end
    }

  // recreate a gap, expanding the buffer as needed
  private def regap(): Unit = if (gapSize == 0) {
    // only regap if gap is empty

    val newSize = math.min(_size.toLong * 3 / 2, Int.MaxValue).toInt

    if (newSize == _size)
      throw new Exception("Buffer is full")

    val newBuffer = Array.ofDim[Char](newSize)

    val endLength = _size - _gapEnd

    System.arraycopy(_buffer, 0, newBuffer, 0, _gapStart)
    System.arraycopy(_buffer, _gapEnd, newBuffer, newSize - endLength, endLength)

    _gapEnd = newSize - endLength

    _buffer = newBuffer

  }

}
