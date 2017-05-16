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
package gnieh.diff

import scala.annotation.tailrec

/** The interface to classes that computes the longest common subsequence between
 *  two sequences of elements
 *
 *  @author Lucas Satabin
 */
abstract class Lcs[T] {

  /** Computes the longest commons subsequence between both inputs.
   *  Returns an ordered list containing the indices in the first sequence and in the second sequence.
   */
  @inline
  def lcs(seq1: IndexedSeq[T], seq2: IndexedSeq[T]): List[Common] =
    lcs(seq1, seq2, 0, seq1.size, 0, seq2.size)

  /** Computest the longest common subsequence between both input slices.
   *  Returns an ordered list containing the indices in the first sequence and in the second sequence.
   *  Before calling the actual lcs algorithm, it performs some preprocessing to detect trivial solutions.
   */
  def lcs(s1: IndexedSeq[T], s2: IndexedSeq[T], low1: Int, high1: Int, low2: Int, high2: Int): List[Common] = {
    val seq1 = s1.slice(low1, high1)
    val seq2 = s2.slice(low2, high2)

    if (seq1.isEmpty || seq2.isEmpty) {
      // shortcut if at least on sequence is empty, the lcs, is empty as well
      Nil
    } else if (seq1 == seq2) {
      // both sequences are equal, the lcs is either of them
      List(Common(low1, low2, seq1.size))
    } else if (seq1.startsWith(seq2)) {
      // the second sequence is a prefix of the first one
      // the lcs is the second sequence
      List(Common(low1, low2, seq2.size))
    } else if (seq2.startsWith(seq1)) {
      // the first sequence is a prefix of the second one
      // the lcs is the first sequence
      List(Common(low1, low2, seq1.size))
    } else {

      // remove common prefix and suffix
      val (prefix, middle1, middle2, suffix) = splitPrefixSuffix(seq1, seq2, low1, low2)

      var acc = suffix.toList

      val psize = prefix.map(_.length).getOrElse(0)
      val size1 = middle1.size
      val size2 = middle2.size

      val (short, long) =
        if (size1 < size2) (middle1, middle2) else (middle2, middle1)
      val idx = long.indexOfSlice(short)
      if (idx >= 0) {
        // speedup if one middle is contained in the other
        if (size1 < size2) {
          acc = Common(low1 + psize, low2 + idx + psize, size1) :: acc
        } else {
          acc = Common(idx + low1 + psize, low2 + psize, size2) :: acc
        }
      } else {
        acc = lcsInner(middle1, low1 + psize, middle2, low2 + psize).foldLeft(acc) { (acc, common) =>
          common :: acc
        }
      }

      acc = prefix.toList ++ acc

      acc
    }

  }

  /** Computest the longest common subsequence between both input slices.
   *  Returns an ordered list containing the indices in the first sequence and in the second sequence.
   */
  def lcsInner(s1: IndexedSeq[T], low1: Int, s2: IndexedSeq[T], low2: Int): List[Common]

  /* Extract common prefix and suffix from both sequences */
  private def splitPrefixSuffix(seq1: IndexedSeq[T], seq2: IndexedSeq[T], low1: Int, low2: Int): (Option[Common], IndexedSeq[T], IndexedSeq[T], Option[Common]) = {
    val size1 = seq1.size
    val size2 = seq2.size
    val size = math.min(size1, size2)
    @tailrec
    def prefixLoop(idx: Int): Option[Common] =
      if (idx >= size || seq1(idx) != seq2(idx)) {
        if (idx == 0) {
          None
        } else {
          Some(Common(low1, low2, idx))
        }
      } else {
        prefixLoop(idx + 1)
      }
    val prefix = prefixLoop(0)
    @tailrec
    def suffixLoop(idx1: Int, idx2: Int, l: Int): Option[Common] =
      if (idx1 < 0 || idx2 < 0 || seq1(idx1) != seq2(idx2)) {
        if (l == 0) {
          None
        } else {
          Some(Common(idx1 + 1, idx2 + 1, l))
        }
      } else {
        suffixLoop(idx1 - 1, idx2 - 1, l + 1)
      }
    val suffix = suffixLoop(size1 - 1, size2 - 1, 0)
    val psize = prefix.map(_.length).getOrElse(0)
    val ssize = suffix.map(_.length).getOrElse(0)
    (prefix, seq1.drop(psize).dropRight(ssize), seq2.drop(psize).dropRight(ssize), suffix)
  }

  protected def push(idx1: Int, idx2: Int, commons: List[Common], back: Boolean): List[Common] =
    commons match {
      case Common(i1, i2, l) :: rest if back && idx1 + 1 == i1 && idx2 + 1 == i2 =>
        Common(idx1, idx2, l + 1) :: rest
      case Common(i1, i2, l) :: rest if i1 + l == idx1 && i2 + l == idx2 =>
        Common(i1, i2, l + 1) :: rest
      case Common(i1, i2, l) :: rest if i1 < idx1 && i2 < idx2 && i1 + l > idx1 && i2 + l > idx2 =>
        commons
      case r =>
        Common(idx1, idx2, 1) :: r
    }

  protected def push(common: Common, commons: List[Common], back: Boolean): List[Common] =
    commons match {
      case Common(i1, i2, l) :: rest if back && common.start1 + common.length >= i1 && common.start2 + common.length >= i2 =>
        Common(common.start1, common.start2, common.length + l - (i1 - common.start1)) :: rest
      case Common(i1, i2, l) :: rest if i1 + l >= common.start1 && i2 + l >= common.start2 =>
        Common(i1, i2, l + common.length - (common.start1 - i1)) :: rest
      case r =>
        common :: r
    }

}

