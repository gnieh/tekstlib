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

import scala.annotation.tailrec

/** Implementation of suffix-array.
 *
 *  Implemented according to
 *  Kärkkäinen, Juha, and Peter Sanders.
 *  "Simple linear work suffix array construction."
 *  Automata, Languages and Programming. Springer Berlin Heidelberg, 2003. 943-955.
 *
 *  Source code translated from C to Scala by Lucas Satabin. Original C sources
 *  were found at <a
 *  href="http://people.mpi-inf.mpg.de/~sanders/programs/suffix/drittel.C"
 *  >http://people.mpi-inf.mpg.de/~sanders/programs/suffix/drittel.C</a>
 *
 *  @author Lucas Satabin
 */
trait SuffixArray {

  def suffixArray(str: String): Vector[Int] = {
    val s = str.toCharArray.map(_.toInt) ++ Array(0, 0, 0)
    val sa = Array.ofDim[Int](s.length)
    val k = s.max
    suffixArray(s, sa, s.length - 3, k)
    sa.toVector.dropRight(3)
  }

  // lexicographical order for pairs
  @inline
  private def leq(a1: Int, a2: Int, b1: Int, b2: Int): Boolean =
    a1 < b1 || (a1 == b1 && a2 <= b2)

  // lexicographical order for triples
  @inline
  private def leq(a1: Int, a2: Int, a3: Int, b1: Int, b2: Int, b3: Int): Boolean =
    a1 < b1 || (a1 == b1 && leq(a2, a3, b2, b3))

  // stably sort a[0..n-1] to b[0..n-1] with keys in 0..K from r
  private def radixPass(a: Array[Int], b: Array[Int], r: Array[Int], roffset: Int, n: Int, k: Int): Unit = {
    // count occurrences
    val c = Array.fill(k + 1)(0)
    for (i <- 0 until n)
      c(r(a(i) + roffset)) += 1
    var sum = 0
    for (i <- 0 to k) {
      val t = c(i)
      c(i) = sum
      sum += t
    }
    for (i <- 0 until n) {
      b(c(r(a(i) + roffset))) = a(i)
      c(r(a(i) + roffset)) += 1
    }
  }

  // find the suffix array SA of s[0..n-1] in {1..K}ˆn
  // require s[n]=s[n+1]=s[n+2]=0, n>=2
  private def suffixArray(s: Array[Int], sa: Array[Int], n: Int, k: Int): Unit = {
    val n0 = (n + 2) / 3
    val n1 = (n + 1) / 3
    val n2 = n / 3
    val n02 = n0 + n2

    val s12 = Array.ofDim[Int](n02 + 3)
    s12(n02) = 0
    s12(n02 + 1) = 0
    s12(n02 + 2) = 0
    val sa12 = Array.ofDim[Int](n02 + 3)
    sa12(n02) = 0
    sa12(n02 + 1) = 0
    sa12(n02 + 2) = 0
    val s0 = Array.ofDim[Int](n0)
    val sa0 = Array.ofDim[Int](n0)

    // generate positions of mod 1 and mod  2 suffixes
    // the "+(n0-n1)" adds a dummy mod 1 suffix if n%3 == 1
    var j = 0
    for {
      i <- 0 until n + (n0 - n1)
      if i % 3 != 0
    } {
      s12(j) = i
      j += 1
    }

    // lsb radix sort the mod 1 and mod 2 triples
    radixPass(s12, sa12, s, 2, n02, k);
    radixPass(sa12, s12, s, 1, n02, k);
    radixPass(s12, sa12, s, 0, n02, k);

    // find lexicographic names of triples
    var name = 0
    var c0 = -1
    var c1 = -1
    var c2 = -1
    for (i <- 0 until n02) {
      if (s(sa12(i)) != c0 || s(sa12(i) + 1) != c1 || s(sa12(i) + 2) != c2) {
        name += 1
        c0 = s(sa12(i))
        c1 = s(sa12(i) + 1)
        c2 = s(sa12(i) + 2)
      }
      if (sa12(i) % 3 == 1) {
        s12(sa12(i) / 3) = name
      } else {
        s12(sa12(i) / 3 + n0) = name
      }
    }

    // recurse if names are not yet unique
    if (name < n02) {
      suffixArray(s12, sa12, n02, name)
      // store unique names in s12 using the suffix array
      for (i <- 0 until n02) {
        s12(sa12(i)) = i + 1
      }
    } else {
      // generate the suffix array of s12 directly
      for (i <- 0 until n02)
        sa12(s12(i) - 1) = i
    }

    // stably sort the mod 0 suffixes from sa12 by their first character
    j = 0
    for {
      i <- 0 until n02
      if sa12(i) < n0
    } {
      s0(j) = 3 * sa12(i)
      j += 1
    }
    radixPass(s0, sa0, s, 0, n0, k)

    // merge sorted sa0 suffixes and sorted sa12 suffixes
    var p = 0
    var t = n0 - n1
    var k_ = 0
    while (k_ < n) {

      @inline def getI = if (sa12(t) < n0) sa12(t) * 3 + 1 else (sa12(t) - n0) * 3 + 2
      val i = getI
      val j = sa0(p)
      val c =
        if (sa12(t) < n0)
          leq(s(i), s12(sa12(t) + n0), s(j), s12(j / 3))
        else
          leq(s(i), s(i + 1), s12(sa12(t) - n0 + 1), s(j), s(j + 1), s12(j / 3 + n0))
      if (c) {
        // suffix from sa12 is smaller
        sa(k_) = i
        t += 1
        if (t == n02) {
          // done --- only sa0 suffixes left
          k_ += 1
          while (p < n0) {
            sa(k_) = sa0(p)
            k_ += 1
            p += 1
          }
        }
      } else {
        // suffix from sa0 is maller
        sa(k_) = j
        p += 1
        if (p == n0) {
          // done --- only sa12 suffixes left
          k_ += 1
          while (t < n02) {
            sa(k_) = getI
            t += 1
            k_ += 1
          }
        }
      }

      k_ += 1
    }

  }

}

object SuffixArray extends SuffixArray
