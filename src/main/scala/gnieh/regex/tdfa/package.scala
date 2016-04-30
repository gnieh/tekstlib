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
package gnieh.regex

import scala.annotation.tailrec

package object tdfa {

  type State = Int

  implicit class RichSeq[T](val seq: Seq[T]) extends AnyVal {

    def foldCut[U](zero: U)(f: (U, T) => (U, Boolean)): U = {
      @tailrec
      def loop(seq: Seq[T], acc: U): U =
        if (seq.isEmpty) {
          acc
        } else {
          val elt = seq.head
          val (acc1, cont) = f(acc, elt)
          if (cont) {
            loop(seq.tail, acc1)
          } else {
            acc1
          }
        }
      loop(seq, zero)
    }

  }

  implicit class RichSet[T](val set: Set[T]) extends AnyVal {

    def foldCut[U](zero: U)(f: (U, T) => (U, Boolean)): U = {
      @tailrec
      def loop(set: Set[T], acc: U): U =
        if (set.isEmpty) {
          acc
        } else {
          val elt = set.head
          val (acc1, cont) = f(acc, elt)
          if (cont) {
            val rest = set - elt
            loop(rest, acc1)
          } else {
            acc1
          }
        }
      loop(set, zero)
    }

    def foldIfAll[U](zero: U)(f: (U, T) => Option[U]): Option[U] = {
      @tailrec
      def loop(set: Set[T], acc: U): Option[U] =
        if (set.isEmpty) {
          Some(acc)
        } else {
          val elt = set.head
          f(acc, elt) match {
            case Some(acc) =>
              val rest = set - elt
              loop(rest, acc)
            case None =>
              None
          }
        }
      loop(set, zero)
    }

  }

}
