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
package matching

import scala.annotation.tailrec

object KMP {

  private sealed trait Result
  private case class Found(idx: Int) extends Result
  private case class Interrupted(idx: Int) extends Result

  private def assertInterrupted(r: Result): Int = r match {
    case Interrupted(j) => j
    case Found(_)       => throw new Exception("This is a bug")
  }

  def search[Coll, T](seq: Coll, pattern: Coll)(implicit indexable: Indexable[Coll, T], equiv: Equiv[T]): Int = {

    @tailrec
    def loop(pattern: Coll, m: Int, seq: Coll, n: Int, table: Vector[Int], j: Int, k: Int): Result =
      if (j == m)
        Found(k - j)
      else if (k == n)
        Interrupted(j)
      else if (pattern(j) ~= seq(k))
        loop(pattern, m, seq, n, table, j + 1, k + 1)
      else if (j == 0)
        loop(pattern, m, seq, n, table, 0, k + 1)
      else
        loop(pattern, m, seq, n, table, table(j), k)

    @tailrec
    def init(j: Int, table: Vector[Int]): Vector[Int] =
      if (j >= pattern.size) {
        table
      } else {
        val s = assertInterrupted(loop(pattern, pattern.size, pattern, j, table, table(j - 1), j - 1))
        init(j + 1, table.updated(j, s))
      }

    val table = init(2, Vector.fill(pattern.size)(0))

    loop(pattern, pattern.size, seq, seq.size, table, 0, 0) match {
      case Found(i)       => i
      case Interrupted(_) => -1
    }
  }

}
