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

import scala.collection.mutable.{
  Set,
  ListBuffer,
  Map
}

class TNfaBuilder[Symbol] {

  private var nextEpsPrio = 0

  private var nextFinalPrio = 0

  private var nextState = 0

  private var initState = 0

  private val finalStates = Map.empty[State, Int]

  private val transitions = Map.empty[State, ListBuffer[Transition[Symbol]]]

  def createState: State = {
    val res = nextState
    nextState += 1
    res
  }

  def makeInitial(state: State): Unit =
    initState = state

  def makeFinal(state: State): Unit =
    if (!finalStates.contains(state)) {
      finalStates(state) = nextFinalPrio
      nextFinalPrio += 1
    }

  def addTransition(from: State, sym: Symbol, to: State): this.type = {
    val trs = transitions.getOrElseUpdate(from, ListBuffer.empty)
    trs += new SymbolTransition(sym, to)
    this
  }

  def addAnyTransition(from: State, to: State): this.type = {
    val trs = transitions.getOrElseUpdate(from, ListBuffer.empty)
    trs += new AnyTransition(to)
    this
  }

  def addEpsTransition(from: State, tags: scala.collection.immutable.Set[Int], to: State): this.type = {
    val trs = transitions.getOrElseUpdate(from, ListBuffer.empty)
    trs += new EpsilonTransition(nextEpsPrio, tags, to)
    nextEpsPrio += 1
    this
  }

  def addEpsTransition(from: State, to: State): this.type = {
    val trs = transitions.getOrElseUpdate(from, ListBuffer.empty)
    trs += new EpsilonTransition(nextEpsPrio, scala.collection.immutable.Set.empty, to)
    nextEpsPrio += 1
    this
  }

  def build: TNfa[Symbol] =
    new TNfa[Symbol](initState, (0 until nextState).toSet, finalStates.toMap, transitions.mapValues(_.toSeq).toMap)

}
