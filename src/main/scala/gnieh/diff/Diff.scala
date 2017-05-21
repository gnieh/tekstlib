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
package gnieh.diff

import scala.annotation.tailrec

class LcsDiff(lcsalg: Lcs) {

  def diff[Coll, T](s1: Coll, s2: Coll)(implicit indexable: Indexable[Coll, T], equiv: Equiv[T]): List[Diff] = {
    val lcs = lcsalg.lcs(s1, s2)
    @tailrec
    def loop(lcs: List[Common], idx1: Int, idx2: Int, acc: List[Diff]): List[Diff] =
      lcs match {
        case Nil =>
          if (idx1 < s1.size && idx2 < s2.size)
            (Second(idx2, s2.size) :: First(idx1, s1.size) :: acc).reverse
          else if (idx1 < s1.size)
            (First(idx1, s1.size) :: acc).reverse
          else if (idx2 < s2.size)
            (Second(idx2, s2.size) :: acc).reverse
          else
            acc.reverse
        case Common(start1, start2, _) :: _ if idx1 < start1 || idx2 < start2 =>
          if (idx1 < start1 && idx2 < start2)
            loop(lcs, start1, start2, Second(idx2, start2) :: First(idx1, start1) :: acc)
          else if (idx1 < start1)
            loop(lcs, start1, idx2, First(idx1, start1) :: acc)
          else
            loop(lcs, idx1, start2, Second(idx2, start2) :: acc)
        case Common(start1, start2, length) :: rest if length > 0 =>
          loop(rest, start1 + length, start2 + length, Both(start1, start1 + length, start2, start2 + length) :: acc)
        case Common(start1, start2, _) :: rest =>
          loop(rest, start1, start2, acc)
      }
    loop(lcs, 0, 0, Nil)
  }

}

sealed trait Diff
final case class First(start: Int, end: Int) extends Diff
final case class Second(start: Int, end: Int) extends Diff
final case class Both(start1: Int, end1: Int, start2: Int, end2: Int) extends Diff
