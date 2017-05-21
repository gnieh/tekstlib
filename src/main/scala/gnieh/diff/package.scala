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

package object diff extends IndexableInstances {

  implicit class IndexableOps[Coll, T](val coll: Coll) extends AnyVal {

    @inline
    def isEmpty(implicit indexable: Indexable[Coll, T]): Boolean =
      indexable.isEmpty(coll)

    @inline
    def startsWith(that: Coll)(implicit indexable: Indexable[Coll, T], equiv: Equiv[T]): Boolean =
      indexable.startsWith(coll, that)

    @inline
    def size(implicit indexable: Indexable[Coll, T]): Int =
      indexable.size(coll)

    @inline
    def slice(start: Int, end: Int)(implicit indexable: Indexable[Coll, T]): Coll =
      indexable.slice(coll, start, end)

    @inline
    def apply(idx: Int)(implicit indexable: Indexable[Coll, T]): T =
      indexable.apply(coll, idx)

    @inline
    def equivalent(that: Coll)(implicit indexable: Indexable[Coll, T], equiv: Equiv[T]): Boolean =
      indexable.equivalent(coll, that)

  }

}
