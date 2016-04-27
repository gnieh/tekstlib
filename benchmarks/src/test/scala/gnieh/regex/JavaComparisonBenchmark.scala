/*
* This file is part of the regex project.
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

import org.scalameter.api._
import org.scalameter.picklers.noPickler._

class JavaComparisonBenchmark extends Bench.LocalTime {

  val gniehReBC = {
    import bytecode._
    "abc|abd|abe|abf".re
  }

  val gniehReDFA = {
    import automaton._
    "abc|abd|abe|abf".re
  }

  val stdRe = "abc|abd|abe|abf"

  val text = Gen.single("text")("fda f ofdio difm i mfofgroa gaabcdjsaabedsakabflklklabdabcabakldskfsdfbacpabc")

  performance of "gnieh regular expressions based on bytecode" in {

    measure method "findAllIn" in {

      using(text) in { t =>
        gniehReBC.isMatchedBy(t)

      }

    }

  }

  performance of "gnieh regular expressions based on TDFA" in {

    measure method "findAllIn" in {

      using(text) in { t =>
        gniehReDFA.isMatchedBy(t)

      }

    }

  }

  performance of "standard regular expressions" in {

    measure method "findAllIn" in {

      using(text) in { t =>
        t.matches(stdRe)

      }

    }

  }

}

