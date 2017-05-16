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

/** Implementation of the LCS using dynamic programming.
 *
 *  @author Lucas Satabin
 */
class DynamicProgLcs[T] extends Lcs[T] {

  def lcsInner(seq1: IndexedSeq[T], low1: Int, seq2: IndexedSeq[T], low2: Int): List[Common] = {
    val lengths = Array.ofDim[Int](seq1.size + 1, seq2.size + 1)
    // fill up the length matrix
    for {
      i <- 0 until seq1.size
      j <- 0 until seq2.size
    } if (seq1(i) == seq2(j))
      lengths(i + 1)(j + 1) = lengths(i)(j) + 1
    else
      lengths(i + 1)(j + 1) = math.max(lengths(i + 1)(j), lengths(i)(j + 1))
    // and compute the lcs out of the matrix
    @tailrec
    def loop(idx1: Int, idx2: Int, acc: List[Common]): List[Common] =
      if (idx1 == 0 || idx2 == 0) {
        acc.reverse
      } else if (lengths(idx1)(idx2) == lengths(idx1 - 1)(idx2)) {
        loop(idx1 - 1, idx2, acc)
      } else if (lengths(idx1)(idx2) == lengths(idx1)(idx2 - 1)) {
        loop(idx1, idx2 - 1, acc)
      } else {
        loop(idx1 - 1, idx2 - 1, push(low1 + idx1 - 1, low2 + idx2 - 1, acc, true))
      }
    loop(seq1.size, seq2.size, Nil)
  }

}
