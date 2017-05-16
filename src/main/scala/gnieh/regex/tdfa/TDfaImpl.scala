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
package tdfa

import util._

import scala.annotation.tailrec

/** This class provides a way to create and use regular expressions. It is a TDFA based implementation.
 *  Following regular expressions are supported:
 *   - `.` any character, possibly including newline (s=true)
 *   - `[xyz]` character class
 *   - `[^xyz]` negated character class
 *   - `\d` a digit character (equivalent to `[0-9]`)
 *   - `\D` a non digit character (equivalent to `[^0-9]`)
 *   - `\w` an alphanumeric character (equivalent to `[A-Za-z0-9_]`)
 *   - `\W` a non alphanumeric character (equivalent to `[^A-Za-z0-9_]`)
 *   - `\s` a space character (equivalent to `[ \t\r\n\f]`)
 *   - `\S` a non space character (equivalent to `[^ \t\r\n\f]`)
 *   - `xy` `x` followed by `y`
 *   - `x|y` `x` or `y` (prefer `x`)
 *   - `x*` zero or more `x` (prefer more)
 *   - `x+` one or more `x` (prefer more)
 *   - `x?` zero or one `x` (prefer one)
 *   - `(re)` numbered capturing group (starting at 1)
 *
 *  @author Lucas Satabin
 */
object TDfaImpl extends RegexImpl {

  type Compiled = TDfa[Char]

  def compile(re: ReNode): (Int, TDfa[Char]) = {
    def loop(currentSave: Int, re: ReNode, builder: TNfaBuilder[Char], lastState: State): (Int, State) = {
      re match {
        case Empty =>
          // match
          builder.makeFinal(lastState)
          (currentSave, lastState)
        case SomeChar(c) =>
          // lastState - c -> newState
          val newState = builder.createState
          builder.addTransition(lastState, c, newState)
          (currentSave, newState)
        case AnyChar =>
          // lastState - * -> newState
          val newState = builder.createState
          builder.addAnyTransition(lastState, newState)
          (currentSave, newState)
        case Concat(e1, e2) =>
          // lastState1 - eps -> comp(e1) - eps -> comp(e2)
          val (currentSave1, lastState1) = loop(currentSave, e1, builder, lastState)
          if (e1.acceptEmpty) {
            builder.makeFinal(lastState1)
          }
          val (currentSave2, lastState2) = loop(currentSave1, e2, builder, lastState1)
          (currentSave2, lastState2)
        case Alt(e1, e2) =>
          // first alternative has higher precedence, so it must be compiled first
          // lastState - eps -> comp(e1) - eps -> newState
          // lastState - eps -> comp(e2) - eps -> newState
          val (currentSave1, lastState1) = loop(currentSave, e1, builder, lastState)
          val (currentSave2, lastState2) = loop(currentSave, e2, builder, lastState)
          val newState = builder.createState
          builder.addEpsTransition(lastState1, newState)
          builder.addEpsTransition(lastState2, newState)
          (currentSave2, newState)
        case Opt(e, true) =>
          // greedy version
          // lastState - eps -> comp(e) - eps -> newState
          // lastState - eps -> newState
          val (currentSave1, lastState1) = loop(currentSave, e, builder, lastState)
          builder.addEpsTransition(lastState, lastState1)
          (currentSave1, lastState1)
        case Opt(e, false) =>
          throw new CompilerException("Unsupported non-greedy optional operator")
        case Star(e, true) =>
          // greedy version
          // lastState - eps -> comp(e) - eps -> lastState
          //                            - eps -> newState
          // lastState - eps -> newState
          val (currentSave1, lastState1) = loop(currentSave, e, builder, lastState)
          builder.addEpsTransition(lastState1, lastState)
          builder.addEpsTransition(lastState, lastState1)
          (currentSave1, lastState1)
        case Star(e, false) =>
          throw new CompilerException("Unsupported non-greedy star operator")
        case Plus(e, true) =>
          // greedy version
          // lastState - eps -> comp(e) - eps -> lastState
          //                            - eps -> newState
          val (currentSave1, lastState1) = loop(currentSave, e, builder, lastState)
          builder.addEpsTransition(lastState1, lastState)
          (currentSave1, lastState1)
        case Plus(e, false) =>
          throw new CompilerException("Unsupported non-greedy plus operator")
        case CharSet(ranges) =>
          // class ranges
          // TODO make better translation, without expanding the ranges
          val newState = builder.createState
          for {
            range <- ranges
            c <- range
          } builder.addTransition(lastState, c, newState)
          (currentSave, newState)
        case Capture(e) =>
          // lastState - eps / tk -> comp(e) - eps tk+1 -> newState
          val newState1 = builder.createState
          builder.addEpsTransition(lastState, Set(currentSave), newState1)
          val (currentSave1, lastState1) = loop(currentSave + 2, e, builder, newState1)
          val newState2 = builder.createState
          builder.addEpsTransition(lastState1, Set(currentSave + 1), newState2)
          (currentSave1, newState2)
        case n =>
          throw new RuntimeException(f"Unsupported regeular expression in TDFA implementation.")
      }
    }
    val builder = new TNfaBuilder[Char]
    val (saved, lastState) = loop(0, re, builder, builder.createState)
    builder.makeFinal(lastState)
    val tdfa = builder.build.determinize
    (saved / 2, tdfa)
  }

  def exec(tdfa: TDfa[Char], nbSaved: Int, startIdx: Int, string: String): (Int, Int, Vector[Int]) = {
    @tailrec
    def loop(currentState: State, currentIdx: Int, tags: Vector[Int], maps: Map[Long, Int]): (Int, Int, Vector[Int]) =
      step(tdfa, currentState, currentIdx, string, tags, maps) match {
        case Some((newState, tags, maps)) =>
          loop(newState, currentIdx + 1, tags, maps)
        case None =>
          tdfa.finishers.get(currentState) match {
            case Some(finishers) =>
              (startIdx, currentIdx, finishers.foldLeft((tags, maps)) {
                case ((tags, maps), c) => c.exec(currentIdx, tags, maps)
              }._1)
            case None =>
              // did not match
              (-1, -1, tags)
          }
      }
    val tags = Vector.fill(nbSaved * 2)(-1)
    val maps = tdfa.initializer.foldLeft(Map.empty[Long, Int]) {
      case (maps, c) =>
        c.exec(startIdx, tags, maps)._2
    }
    loop(tdfa.initialState, startIdx, tags, maps)
  }

  private def step(tdfa: TDfa[Char], currentState: State, currentIdx: Int, string: String, tags: Vector[Int], maps: Map[Long, Int]): Option[(State, Vector[Int], Map[Long, Int])] =
    if (currentIdx < string.size) {
      println(f"Step with character ${string(currentIdx)} at index $currentIdx in state $currentState")
      for ((target, commands) <- tdfa.step(currentState, string.charAt(currentIdx))) yield {
        val (tags1, maps1) =
          commands.foldLeft((tags, maps)) {
            case ((tags, maps), c) => c.exec(currentIdx + 1, tags, maps)
          }
        (target, tags1, maps1)
      }
    } else {
      None
    }

}

class CompilerException(msg: String) extends Exception(msg)
