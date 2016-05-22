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

  def lcsInner(seq1: IndexedSeq[T], low1: Int, seq2: IndexedSeq[T], low2: Int): List[Common] = {
    val size1 = seq1.size
    val size2 = seq2.size
    val max = 1 + size1 + size2
    val v = Array.ofDim[Int](2 * max + 1)
    var acc = List.empty[Common]
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
        acc = push(x + low1, y + low2, acc, false)
        x += 1
        y += 1
      }
      v(max + k) = x
      if (x >= size1 && y >= size2)
        return acc
    }
    throw new Exception("This should never happen")
  }

}
