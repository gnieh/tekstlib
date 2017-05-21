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
package gnieh.diff

import scala.language.higherKinds

trait Indexable[Coll, Elem] {

  def apply(coll: Coll, idx: Int): Elem

  def isEmpty(coll: Coll): Boolean

  def size(coll: Coll): Int

  def slice(coll: Coll, start: Int, end: Int): Coll

  def indexOfSlice(coll: Coll, slice: Coll): Int

  def startsWith(coll: Coll, that: Coll): Boolean
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

    @inline
    def indexOfSlice(coll: String, slice: String): Int =
      coll.indexOfSlice(slice)

    @inline
    def startsWith(s1: String, s2: String): Boolean =
      s1.startsWith(s2)

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

    @inline
    def indexOfSlice(coll: IndexedSeq[T], slice: IndexedSeq[T]): Int =
      coll.indexOfSlice(slice)

    @inline
    def startsWith(s1: IndexedSeq[T], s2: IndexedSeq[T]): Boolean =
      s1.startsWith(s2)

  }

}
