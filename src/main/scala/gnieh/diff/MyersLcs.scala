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

import scala.collection.mutable.ListBuffer

class MyersLcs[T] extends Lcs[T] {

  def lcs(s1: IndexedSeq[T], s2: IndexedSeq[T], low1: Int, high1: Int, low2: Int, high2: Int): List[(Int, Int)] = {
    val seq1 = s1.slice(low1, high1)
    val seq2 = s2.slice(low2, high2)

    if (seq1.isEmpty || seq2.isEmpty) {
      // shortcut if at least on sequence is empty, the lcs, is empty as well
      Nil
    } else if (seq1 == seq2) {
      // both sequences are equal, the lcs is either of them
      seq1.indices.map(i => (i + low1, i + low2)).toList
    } else if (seq1.startsWith(seq2)) {
      // the second sequence is a prefix of the first one
      // the lcs is the second sequence
      seq2.indices.map(i => (i + low1, i + low2)).toList
    } else if (seq2.startsWith(seq1)) {
      // the first sequence is a prefix of the second one
      // the lcs is the first sequence
      seq1.indices.map(i => (i + low1, i + low2)).toList
    } else {

      // remove common prefix and suffix
      val (prefix, middle1, middle2, suffix) = splitPrefixSuffix(seq1, seq2, low1, low2)

      // the buffer of common indices
      val buffer = ListBuffer.empty[(Int, Int)]
      buffer ++= prefix

      val size1 = middle1.size
      val size2 = middle2.size

      val (short, long) =
        if (size1 < size2) (middle1, middle2) else (middle2, middle1)
      val idx = long.indexOfSlice(short)
      if (idx >= 0) {
        // speedup if one middle is contained in the other
        for (i <- 0 until short.size) {
          if (size1 < size2) {
            buffer.append((i + low1 + prefix.size, i + idx + low2 + prefix.size))
          } else {
            buffer.append((i + idx + low1 + prefix.size, i + low2 + prefix.size))
          }
        }
      } else {
        buffer ++= myers(middle1, low1 + prefix.size, middle2, low2 + prefix.size)
      }

      buffer ++= suffix

      buffer.toList
    }

  }

  /* Extract common prefix and suffix from both sequences */
  private def splitPrefixSuffix(seq1: IndexedSeq[T], seq2: IndexedSeq[T], low1: Int, low2: Int): (List[(Int, Int)], IndexedSeq[T], IndexedSeq[T], List[(Int, Int)]) = {
    val size1 = seq1.size
    val size2 = seq2.size
    val size = math.min(size1, size2)
    @tailrec
    def prefixLoop(idx: Int, acc: List[(Int, Int)]): List[(Int, Int)] =
      if (idx >= size || seq1(idx) != seq2(idx)) {
        acc.reverse
      } else {
        prefixLoop(idx + 1, (idx + low1, idx + low2) :: acc)
      }
    val prefix = prefixLoop(0, Nil)
    @tailrec
    def suffixLoop(idx1: Int, idx2: Int, acc: List[(Int, Int)]): List[(Int, Int)] =
      if (idx1 < 0 || idx2 < 0 || seq1(idx1) != seq2(idx2)) {
        acc.reverse
      } else {
        suffixLoop(idx1 - 1, idx2 - 1, (idx1, idx2) :: acc)
      }
    val suffix = suffixLoop(size1 - 1, size2 - 1, Nil)
    (prefix, seq1.drop(prefix.size).dropRight(suffix.size), seq2.drop(prefix.size).dropRight(suffix.size), suffix)
  }

  private def myers(seq1: IndexedSeq[T], low1: Int, seq2: IndexedSeq[T], low2: Int): ListBuffer[(Int, Int)] = {
    val size1 = seq1.size
    val size2 = seq2.size
    val max = 1 + size1 + size2
    val v = Array.ofDim[Int](2 * max + 1)
    val buffer = ListBuffer.empty[(Int, Int)]
    v(max + 1) = 0
    for {
      d <- 0 until max
      k <- -d to d by 2
    } {
      var x =
        if (k == -d || (k != d && v(max + k - 1) < v(max + k + 1)))
          v(max + k + 1)
        else
          v(max + k - 1) + 1
      var y = x - k
      while (x < size1 && y < size2 && seq1(x) == seq2(y)) {
        buffer.append((x + low1, y + low2))
        x += 1
        y += 1
      }
      v(max + k) = x
      if (x >= size1 && y >= size2)
        return buffer
    }
    throw new Exception("This should never happen")
  }

}
