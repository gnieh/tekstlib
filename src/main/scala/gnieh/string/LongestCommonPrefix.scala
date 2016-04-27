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
package gnieh.string

/** Implementation of the longest common prefix among suffixes.
 *
 *  Implemented accorting to
 *  iKasai, Toru, et al.
 *  "Linear-time longest-common-prefix computation in suffix arrays and its applications."
 *  Combinatorial Pattern Matching. Springer Berlin Heidelberg, 2001.
 *
 *  @author Lucas Satabin
 */
trait LongestCommonPrefix {

  /** Returns the list of LCPs where at each index `i`  is stored the the LCP
   *  between suffix starting at `i` and suffix starting at `i - 1`.
   */
  def lcps(s: String, sa: Vector[Int]): Vector[Int] = {

    val n = sa.length
    val rank = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      rank(sa(i)) = i
    }

    val s1 = s + 0.toChar
    val height = Array.ofDim[Int](n)
    var h = 0
    for (i <- 0 until n) {
      if (rank(i) > 0) {
        val k = sa(rank(i) - 1)
        while (s1(i + h) == s1(k + h)) {
          h += 1
        }
        height(rank(i)) = h
        if (h > 0) {
          h -= 1
        }
      }
    }

    height.toVector
  }

}

object LongestCommonPrefix extends LongestCommonPrefix
