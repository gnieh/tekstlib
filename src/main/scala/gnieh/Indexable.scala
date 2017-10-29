/*
* Copyright (c) 2017 Lucas Satabin
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

import matching._

import scala.language.higherKinds

import scala.annotation.tailrec

abstract class Indexable[Coll, Elem] {

  private implicit val self = this

  def apply(coll: Coll, idx: Int): Elem

  def isEmpty(coll: Coll): Boolean

  def size(coll: Coll): Int

  def slice(coll: Coll, start: Int, end: Int): Coll

  def indexOfSlice(s1: Coll, s2: Coll)(implicit equiv: Equiv[Elem]): Int =
    KMP.search(s1, s2)

  def startsWith(s1: Coll, s2: Coll)(implicit equiv: Equiv[Elem]): Boolean = {
    @tailrec
    def loop(idx: Int): Boolean =
      if (idx < size(s1) && idx < size(s2)) {
        if (equiv.equiv(apply(s1, idx), apply(s2, idx))) {
          loop(idx + 1)
        } else {
          false
        }
      } else if (idx >= size(s2)) {
        true
      } else {
        false
      }
    loop(0)
  }

  def equivalent(coll: Coll, that: Coll)(implicit equiv: Equiv[Elem]): Boolean = {
    val s1 = size(coll)
    val s2 = size(that)

    if (s1 == s2) {
      // same size, there is a chance they are equivalent
      @tailrec
      def loop(idx: Int): Boolean =
        if (idx < s1) {
          if (equiv.equiv(apply(coll, idx), apply(that, idx))) {
            // elements are equivalent, continue
            loop(idx + 1)
          } else {
            // non equivalent elements, stop
            false
          }
        } else {
          // end of collection for both, they are equivalent
          true
        }
      loop(0)
    } else {
      false
    }
  }

}

trait IndexableInstances {

  implicit object IndexableString extends Indexable[String, Char] {

    @inline
    def apply(s: String, idx: Int) =
      s.charAt(idx)

    @inline
    def isEmpty(s: String) =
      s == null || s.length == 0

    @inline
    def size(s: String): Int =
      s.length

    @inline
    def slice(s: String, start: Int, end: Int) =
      s.slice(start, end)

  }

  implicit def IndexableIndexedSeq[T]: Indexable[IndexedSeq[T], T] = new Indexable[IndexedSeq[T], T] {

    @inline
    def apply(s: IndexedSeq[T], idx: Int) =
      s(idx)

    @inline
    def isEmpty(s: IndexedSeq[T]) =
      s.isEmpty

    @inline
    def size(s: IndexedSeq[T]): Int =
      s.size

    @inline
    def slice(s: IndexedSeq[T], start: Int, end: Int) =
      s.slice(start, end)

  }

}
