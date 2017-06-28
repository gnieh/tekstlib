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

class Diff3[T](lcs: Lcs[T]) {

  def diff3(left: IndexedSeq[T], original: IndexedSeq[T], right: IndexedSeq[T]): List[Hunk] = {
    @tailrec
    def loop(diffLeft: List[Diff], diffRight: List[Diff], acc: List[Hunk]): List[Hunk] =
      (diffLeft, diffRight) match {
        case (Nil, Nil) => acc.reverse
        case (Nil, _)   => (toHunk(Nil, diffRight) reverse_::: acc).reverse
        case (_, Nil)   => (toHunk(diffLeft, Nil) reverse_::: acc).reverse
        case (_, _) =>
          val (conflictHunk, d1, d2) = shortestConflict(diffLeft, diffRight)
          val (matchHunk, d11, d21) = shortestMatch(d1, d2)
          loop(d11, d21, matchHunk reverse_::: conflictHunk reverse_::: acc)
      }
    val diff = new LcsDiff(lcs)
    loop(diff.diff(original, left), diff.diff(original, right), Nil)
  }

  def merge(hunks: List[Hunk]): Either[List[Hunk], List[Elements]] = {
    @tailrec
    def loop(hunks: List[Hunk], acc: List[Elements]): Option[List[Elements]] =
      hunks match {
        case Nil                             => Some(acc.reverse)
        case Conflict(_, _, _, _, _, _) :: _ => None
        case LeftChange(s, e) :: rest        => loop(rest, FromLeft(s, e) :: acc)
        case RightChange(s, e) :: rest       => loop(rest, FromRight(s, e) :: acc)
        case Unchanged(s, e) :: rest         => loop(rest, FromOrigin(s, e) :: acc)
      }
    loop(hunks, Nil).map(Right(_)).getOrElse(Left(hunks))
  }

  private def toHunk(diff1: List[Diff], diff2: List[Diff]): List[Hunk] =
    (diff1, diff2) match {
      case (Nil, Nil)                                       => Nil
      case (Nil, _)                                         => List(RightChange.tupled(takeSecond(diff2)))
      case (_, Nil)                                         => List(LeftChange.tupled(takeSecond(diff1)))
      case (List(Both(s, e, _, _)), List(Both(_, _, _, _))) => List(Unchanged(s, e))
      case (List(Both(_, _, _, _)), _)                      => List(RightChange.tupled(takeSecond(diff2)))
      case (_, List(Both(_, _, _, _)))                      => List(LeftChange.tupled(takeSecond(diff1)))
      case (_, _) =>
        val (leftStart, leftEnd) = takeSecond(diff1)
        val (originalStart, originalEnd) = takeFirst(diff1)
        val (rightStart, rightEnd) = takeSecond(diff2)
        List(Conflict(leftStart, leftEnd, originalStart, originalEnd, rightStart, rightEnd))
    }

  private def takeFirst(diff: List[Diff]): (Int, Int) =
    diff.foldLeft(-1 -> -1) {
      case ((-1, _), First(s, e))      => (s, e)
      case ((-1, _), Both(s, e, _, _)) => (s, e)
      case ((s, _), First(_, e))       => (s, e)
      case ((s, _), Both(_, e, _, _))  => (s, e)
      case (acc, _)                    => acc
    }

  private def takeSecond(diff: List[Diff]): (Int, Int) =
    diff.foldLeft(-1 -> -1) {
      case ((-1, _), Second(s, e))     => (s, e)
      case ((-1, _), Both(_, _, s, e)) => (s, e)
      case ((s, _), Second(_, e))      => (s, e)
      case ((s, _), Both(_, _, _, e))  => (s, e)
      case (acc, _)                    => acc
    }

  private def shortestMatch(diff1: List[Diff], diff2: List[Diff]): (List[Hunk], List[Diff], List[Diff]) = {
    @tailrec
    def loop(diff1: List[Diff], diff2: List[Diff], acc1: List[Diff], acc2: List[Diff]): (List[Hunk], List[Diff], List[Diff]) =
      (diff1, diff2) match {
        case ((d1 @ Both(_, _, _, _)) :: ds1, (d2 @ Both(_, _, _, _)) :: ds2) => loop(ds1, ds2, d1 :: acc1, d2 :: acc2)
        case (_, _) => (toHunk(diff1, diff2), acc1.reverse, acc2.reverse)
      }
    loop(diff1, diff2, Nil, Nil)
  }

  private def shortestConflict(diff1: List[Diff], diff2: List[Diff]): (List[Hunk], List[Diff], List[Diff]) = {
    @tailrec
    def loop(diff1: List[Diff], diff2: List[Diff], prefixAcc1: List[Diff], prefixAcc2: List[Diff], suffixAcc1: List[Diff], suffixAcc2: List[Diff]): ((List[Diff], List[Diff]), List[Diff], List[Diff]) =
      (diff1, diff2) match {
        case (Nil, _) => ((prefixAcc1.reverse, (diff2 reverse_::: prefixAcc2).reverse), suffixAcc1.reverse, suffixAcc2.reverse)
        case (_, Nil) => (((diff1 reverse_::: prefixAcc1), prefixAcc2.reverse), suffixAcc1.reverse, suffixAcc2.reverse)
        case (_, _) =>
          val (prefix1, suffix1) = diff1.span(d => !isBoth(d))
          val (prefix2, suffix2) = diff2.span(d => !isBoth(d))
          val motion1 = prefix1.map(motion(_)).sum
          val motion2 = prefix2.map(motion(_)).sum
          if (motion1 == motion2) {
            (((prefix1 reverse_::: prefixAcc1).reverse, (prefix2 reverse_::: prefixAcc2).reverse), (suffix1 reverse_::: suffixAcc1).reverse, (suffix2 reverse_::: suffixAcc2).reverse)
          } else {
            val (prefix11, suffix11) = moveInSecond(motion2, suffix1, Nil, Nil)
            val (prefix21, suffix21) = moveInSecond(motion1, suffix2, Nil, Nil)
            loop(suffix11, suffix21, prefix11 reverse_::: prefix1 reverse_::: prefixAcc1, prefix21 reverse_::: prefix2 reverse_::: prefixAcc2, suffix11 reverse_::: suffixAcc1, suffix21 reverse_::: suffixAcc2)
          }
      }
    val ((prefix1, prefix2), suffix1, suffix2) = loop(diff1, diff2, Nil, Nil, Nil, Nil)
    (toHunk(prefix1, prefix2), suffix1, suffix2)
  }

  private def isBoth(d: Diff): Boolean = d match {
    case Both(_, _, _, _) => true
    case _                => false
  }

  private def motion(d: Diff): Int = d match {
    case Second(_, _)     => 0
    case First(s, e)      => e - s
    case Both(s, e, _, _) => e - s
  }

  @tailrec
  private def moveInSecond(n: Int, diff: List[Diff], d1: List[Diff], d2: List[Diff]): (List[Diff], List[Diff]) =
    (n, diff) match {
      case (_, Nil) =>
        (d1.reverse, d2.reverse)
      case (0, _) =>
        (d1.reverse, (diff reverse_::: d2).reverse)
      case (_, (d @ Both(s1, e1, s2, e2)) :: ds) =>
        if (s1 - e1 <= n) {
          moveInSecond(n - (s1 - e1), ds, d :: d1, d2)
        } else {
          moveInSecond(0, ds, Both(s1, s1 + n, s2, s2 + n) :: d1, Both(s1 + n, e1, s2 + n, e2) :: d2)
        }
      case (_, (d @ Second(s, e)) :: ds) =>
        if (s - e <= n) {
          moveInSecond(n - (s - e), ds, d :: d1, d2)
        } else {
          moveInSecond(0, ds, Second(s, s + n) :: d1, Second(s + n, e) :: d2)
        }
      case (_, d :: ds) =>
        moveInSecond(n, ds, d :: d1, d2)
    }

}

sealed trait Hunk
final case class LeftChange(start: Int, end: Int) extends Hunk
final case class RightChange(start: Int, end: Int) extends Hunk
final case class Unchanged(start: Int, end: Int) extends Hunk
final case class Conflict(leftStart: Int, leftEnd: Int, originalStart: Int, originalEnd: Int, rightStart: Int, rightEnd: Int) extends Hunk

sealed trait Elements
final case class FromOrigin(start: Int, end: Int) extends Elements
final case class FromLeft(start: Int, end: Int) extends Elements
final case class FromRight(start: Int, end: Int) extends Elements
