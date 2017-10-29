/*
* Copyright (c) 2016 Lucas Satabin
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
package matching

import scala.annotation.tailrec

import scodec.bits._

/** Implementation of the Bitap algorithm working on any indexed sequence.
 *
 *  @author Lucas Satabin
 */
class Bitap {

  /** Searches for the exact given `pattern` in sequence `s` and returns
   *  the index of the first match or `-1`. Maximum `k` errors are allowed.
   */
  def search[Coll, T](pattern: Coll, s: Coll, from: Int = 0, k: Int = 0)(implicit indexable: Indexable[Coll, T], equiv: Equiv[T]): Int = {

    class Wrapped(val value: T) {
      override def hashCode = value.hashCode
      override def equals(o: Any) = o match {
        case that: Wrapped => equiv.equiv(this.value, that.value)
        case _             => false
      }
    }

    val m = pattern.size
    val n = s.size
    val notOne = BitVector.high(m + 1).update(m, false)
    val r = Vector.fill(k + 1)(notOne)
    val one = BitVector.low(m + 1).update(m, true)

    def patternMap[Coll, T]: Map[Wrapped, BitVector] = {
      @tailrec
      def loop(idx: Int, acc: Map[Wrapped, BitVector]): Map[Wrapped, BitVector] =
        if (idx == m) {
          acc
        } else {
          val t = new Wrapped(pattern(idx))
          loop(idx + 1, acc.updated(t, acc(t) & ~(one << idx)))
        }
      val ones = BitVector.high(m + 1)
      loop(0, Map.empty.withDefaultValue(ones))
    }

    val patternMask = patternMap

    val oneShiftM = BitVector.low(m + 1).update(0, true)
    val zeros = BitVector.low(m + 1)

    @tailrec
    def loop(idx: Int, r: Vector[BitVector]): Int =
      if (idx >= n) {
        -1
      } else {
        val t = s(idx)
        val r01 = r(0) | patternMask(new Wrapped(t))
        val r02 = r01 << 1

        val r1 = r.updated(0, r02)

        @tailrec
        def dloop(d: Int, r: Vector[BitVector], old: BitVector, nextOld: BitVector): Vector[BitVector] =
          if (d > k) {
            r
          } else {
            val t1 = new Wrapped(t)
            val sub = (old & (r(d) | patternMask(t1))) << 1
            val ins = old & ((r(d) | patternMask(t1)) << 1)
            val del = (nextOld & (r(d) | patternMask(t1))) << 1
            val r1 = r.updated(d, sub & ins & del)
            dloop(d + 1, r1, r(d), r1(d))
          }

        val r2 = dloop(1, r1, r(0), r1(0))

        if (zeros == (r2(k) & oneShiftM)) {
          idx - m + 1
        } else {
          loop(idx + 1, r2)
        }
      }
    loop(from, r)
  }

}

object Bitap extends Bitap
