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

class TDfa[Symbol](val initialState: State,
    val states: Set[State],
    val initializer: Vector[Command],
    val finishers: Map[State, Vector[Command]],
    val transitions: Map[State, Map[Symbol, (State, Vector[Command])]],
    val fallbacks: Map[State, (State, Vector[Command])]) {

  def step(state: State, sym: Symbol): Option[(State, Vector[Command])] =
    transitions.get(state).flatMap(_.get(sym)).orElse(fallbacks.get(state))

  def toDot: String = {
    val result = new StringBuilder
    result.append("digraph TDFA {\n")
    result.append("  rankdir = LR\n")

    result.append("  initial [shape=plaintext,label=\"start\"]\n")
    result.append(f"  initial -> $initialState[label=<${initializer.mkString("\\n")}>]\n")

    for (state <- states) {
      if (finishers.contains(state)) {
        result.append(f"""  $state[shape=doublecircle,label="$state"]\n""")
        result.append(f"""  final$state [shape=plaintext,label="end"]\n""")
        result.append(f"$state->final$state[label=<${finishers(state).mkString("\n")}>]\n")
      } else {
        result.append(f"""  $state[shape=circle,label="$state"]\n""")
      }

      for {
        trs <- transitions.get(state)
        (sym, (target, commands)) <- trs
      } result.append(f"$state->$target[label=<$sym / ${commands.mkString("<br/>")}>]\n")

      for ((target, commands) <- fallbacks.get(state))
        result.append(f"$state->$target[label=<<i>fallback</i> / ${commands.mkString("<br/>")}>]\n")

    }

    result.append("}")

    result.toString
  }

}
