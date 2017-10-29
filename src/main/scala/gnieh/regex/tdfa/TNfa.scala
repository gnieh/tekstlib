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

import scala.annotation.tailrec

import scala.collection.{ mutable => mu }

sealed trait Transition[+Symbol]
final case class EpsilonTransition(priority: Int, tags: Set[Int], target: State) extends Transition[Nothing]
final case class SymbolTransition[Symbol](sym: Symbol, target: State) extends Transition[Symbol]
final case class AnyTransition(target: State) extends Transition[Nothing]

class TNfa[Symbol](
    val initialState: State,
    val states: Set[State],
    val finalStates: Map[State, Int],
    val transitions: Map[State, Seq[Transition[Symbol]]]) {

  private sealed trait ReadSymbol {
    def accepts(s: Symbol): Boolean
  }

  private case class SomeSymbol(sym: Symbol) extends ReadSymbol {

    def accepts(s: Symbol): Boolean =
      s == sym

  }

  private case class AnySymbol(except: Set[Symbol]) extends ReadSymbol {

    def accepts(s: Symbol): Boolean =
      !except.contains(s)

  }

  def toDot: String = {
    val result = new StringBuilder
    result.append("digraph NFA {\n")
    result.append("  rankdir = LR\n")

    result.append("  initial [shape=plaintext,label=\"\"]\n")
    result.append(f"  initial -> $initialState\n")

    for (state <- states) {
      if (finalStates.contains(state)) {
        result.append(f"""  $state[shape=doublecircle,label="$state"]\n""")
      } else {
        result.append(f"""  $state[shape=circle,label="$state"]\n""")
      }

      for {
        trs <- transitions.get(state)
        tr <- trs
      } tr match {
        case SymbolTransition(sym, target) =>
          result.append(f"""  $state->$target[label="$sym"]
                           |""".stripMargin)
        case AnyTransition(target) =>
          result.append(f"""  $state->$target[label="★"]
                          |""".stripMargin)
        case EpsilonTransition(prio, tags, target) =>
          result.append(f"""  $state->$target[label=<ε<sup>$prio</sup> / ${tags.map(t => f"t<sub>$t</sub>").mkString(", ")}>]
                           |""".stripMargin)
      }
    }

    result.append("}")

    result.toString
  }

  def determinize(): TDfa[Symbol] = {

    // the initial TDFA state is the epsilon closure of the initial TNFA state
    val initialClosure = epsilonClosure(Map(initialState -> Seq.empty))

    // the initializer contains all the commands to set positions to initial tags
    val initializer =
      for {
        (_, ms) <- initialClosure
        m <- ms
      } yield Assign(m, CurrentPosition)

    val states = mu.Set(initialClosure)
    val unmarked = mu.Queue(initialClosure)
    val tnfa2tdfa = mu.Map(initialClosure -> 0)
    var nextTdfaState = 1
    val transitions = mu.Map.empty[State, mu.Map[ReadSymbol, (State, Vector[Command])]]
    val finishers = mu.Map.empty[State, Vector[Command]]

    while (unmarked.nonEmpty) {
      val t = unmarked.dequeue
      for (sym <- alphabet(t)) {
        val reached = reach(t, sym)
        val u = epsilonClosure(reached)
        val c = for {
          (_, m) <- u.toVector
          TagMap(n, i) <- m
          if !containsMapping(reached, n, i)
        } yield Assign(TagMap(n, i), CurrentPosition)
        reorder(u, states.toSet) match {
          case Some((u1, cs)) =>
            val st = tnfa2tdfa(u1)
            val c1 = c ++ cs
            transitions.getOrElseUpdate(tnfa2tdfa(t), mu.Map.empty)(sym) = (st, c1.distinct)
          case None =>
            states += u
            unmarked.enqueue(u)
            tnfa2tdfa(u) = nextTdfaState
            transitions.getOrElseUpdate(tnfa2tdfa(t), mu.Map.empty)(sym) = (nextTdfaState, c.distinct)

            val finalState =
              u.foldLeft(Option.empty[(State, Seq[TagMap], Int)]) {
                case (acc @ Some((_, _, prio)), (s, k)) =>
                  finalStates.get(s) match {
                    case Some(p) if p < prio => Some((s, k, p))
                    case _                   => acc
                  }
                case (None, (s, k)) =>
                  finalStates.get(s).map((s, k, _))
              }
            for ((state, maps, _) <- finalState) {
              // add the finisher for u1 which consists in assigning tags based on the smallest available index for this tag

              // sort the tag maps so that the first tag map we encounter for a given tag is the one we want
              val (_, finisher) =
                maps.sorted.foldLeft(Set.empty[Int] -> Vector.empty[Command]) {
                  case (acc @ (seen, commands), map) =>
                    if (seen.contains(map.id)) {
                      acc
                    } else {
                      (seen + map.id, commands :+ Assign(Tag(map.id), map))
                    }
                }
              finishers(nextTdfaState) = finisher.distinct
            }
            nextTdfaState += 1
        }
      }
    }
    val (transitions1, fallbacks) =
      transitions.foldLeft((Map.empty[State, Map[Symbol, (State, Vector[Command])]].withDefaultValue(Map.empty), Map.empty[State, (State, Vector[Command])])) {
        case (acc, (state, trs)) =>
          trs.foldLeft(acc) {
            case ((accTransitions, accFallbacks), (SomeSymbol(sym), tr)) =>
              (accTransitions.updated(state, accTransitions(state).updated(sym, tr)), accFallbacks)
            case ((accTransitions, accFallbacks), (AnySymbol(_), tr)) =>
              (accTransitions, accFallbacks.updated(state, tr))
          }
      }
    new TDfa(0, tnfa2tdfa.values.toSet, initializer.toVector, finishers.toMap, transitions1, fallbacks)
  }

  private def alphabet(states: Map[State, Seq[TagMap]]): Set[ReadSymbol] = {
    val (hasAny, syms) =
      states.keySet.foldLeft((false, Set.empty[Symbol])) {
        case (acc, state) =>
          transitions.get(state) match {
            case Some(trs) =>
              trs.foldLeft(acc) {
                case ((accHasAny, accSyms), SymbolTransition(s, _)) =>
                  (accHasAny, accSyms + s)
                case ((_, accSyms), AnyTransition(_)) =>
                  (true, accSyms)
                case (acc, _) =>
                  acc
              }
            case None =>
              acc
          }
      }
    val symSet: Set[ReadSymbol] = syms.map(SomeSymbol(_))
    if (hasAny) {
      symSet + AnySymbol(syms)
    } else {
      symSet
    }
  }

  @tailrec
  private def reorder(from: Map[State, Seq[TagMap]], to: Set[Map[State, Seq[TagMap]]]): Option[(Map[State, Seq[TagMap]], Vector[Command])] =
    if (to.nonEmpty) {
      val toState = to.head
      val rest = to - toState
      if (from.keySet == toState.keySet) {
        val commands =
          from.keySet.foldIfAll(Vector.empty[Command]) {
            case (acc, key) =>
              val fromMaps = from(key).groupBy(_.id)
              val toMaps = toState(key).groupBy(_.id)
              if (fromMaps.keySet == toMaps.keySet) {
                fromMaps.keySet.foldIfAll(acc) {
                  case (acc, tag) =>
                    val fromTag = fromMaps(tag).sorted
                    val toTag = toMaps(tag).sorted
                    if (fromTag.size == toTag.size) {
                      val reordering =
                        for {
                          (TagMap(_, fromIdx), TagMap(_, toIdx)) <- fromTag.zip(toTag)
                          if fromIdx != toIdx
                        } yield Reorder(tag, fromIdx, toIdx)
                      Some(reordering.toVector ++ acc)
                    } else {
                      None
                    }
                }
              } else {
                None
              }
          }
        if (commands.nonEmpty) {
          commands.map(toState -> _)
        } else {
          reorder(from, rest)
        }
      } else {
        reorder(from, rest)
      }
    } else {
      None
    }

  private def containsMapping(state: Map[State, Seq[TagMap]], tag: Int, index: Int): Boolean = {
    @tailrec
    def loop(state: Map[State, Seq[TagMap]]): Boolean =
      if (state.isEmpty) {
        false
      } else {
        val (s, m) = state.head
        if (m.exists(_ == TagMap(tag, index))) {
          true
        } else {
          loop(state - s)
        }
      }
    loop(state)
  }

  /** Returns the transitions from the given state, accepting the given symbol */
  private def transitions(state: State, sym: ReadSymbol): Set[State] =
    transitions.get(state) match {
      case Some(trs) =>
        trs.foldLeft(Set.empty[State]) {
          case (acc, SymbolTransition(s, target)) if sym.accepts(s) =>
            acc + target
          case (acc, AnyTransition(target)) =>
            acc + target
          case (acc, _) =>
            acc
        }
      case None =>
        Set.empty
    }

  private def epsilonTransitions(state: State): Seq[(Set[Int], Int, State)] =
    transitions.get(state) match {
      case Some(trs) =>
        for (EpsilonTransition(prio, tags, target) <- trs)
          yield (tags, prio, target)
      case None =>
        Seq.empty
    }

  /** Compute the set of states reached from `t` reading `sym` with the priority of each transition taken and the set of tags emitted. */
  private def reach(t: Map[State, Seq[TagMap]], sym: ReadSymbol): Map[State, Seq[TagMap]] =
    t.foldLeft(Map.empty[State, Seq[TagMap]]) {
      case (acc, (state, maps)) =>
        transitions(state, sym).foldLeft(acc) {
          case (acc, target) =>
            acc.updated(target, maps)
        }
    }

  private def epsilonClosure(pairs: Map[State, Seq[TagMap]]): Map[State, Seq[TagMap]] = {

    val stack = mu.Queue.empty[(State, Seq[TagMap])]
    val closure = mu.Set.empty[(State, Int, Seq[TagMap])]
    for (p @ (u, k) <- pairs) {
      stack.enqueue(p)
      closure += ((u, 0, k))
    }

    while (stack.nonEmpty) {
      // we still have states to process
      val (s, k) = stack.dequeue
      // for each epsilon transition originating from state s with priority p and emitting tags
      for ((tags, p, u) <- epsilonTransitions(s)) {
        // for each emitted tag
        val k1 =
          tags.foldLeft(k) {
            case (k, tag) =>
              val tm = TagMap(tag, nextIndex(pairs, tag))
              // only emit tag t once in k
              val k1 = k.filterNot(_.id == tag)
              k1 :+ tm
          }
        // if the closure already contains target state u with a lower priority, replace it by this new one
        for {
          v @ (u1, p1, _) <- closure
          if u1 == u && p1 > p
        } closure - v

        if (!closure.contains((u, p, k1))) {
          closure += ((u, p, k1))
          stack.enqueue(u -> k1)
        }
      }
    }

    closure.foldLeft(Map.empty[State, Seq[TagMap]].withDefaultValue(Seq.empty)) {
      case (acc, (s, _, k)) => acc.updated(s, (acc(s) ++ k).distinct)
    }
  }

  private def nextIndex(pairs: Map[State, Seq[TagMap]], tag: Int): Int = {
    pairs.foldLeft(-1) {
      case (lastIndex, (_, m)) =>
        m.find(_.id == tag) match {
          case Some(tm) => math.max(lastIndex, tm.index)
          case None     => lastIndex
        }
    } + 1
  }

}
