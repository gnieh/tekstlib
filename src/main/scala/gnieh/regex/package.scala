/*
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

import scala.collection.immutable.NumericRange

package object regex {

  type CharClass = NumericRange[Char]

  /** The non backtracking, VM implementation of regular expression. Import this object to use it. */
  object bytecode {

    implicit val impl = vm.BytecodeImpl

    implicit class RegexString(val string: String) extends AnyVal {

      def re: Regex =
        Regex(string)

    }

    implicit class RegexContext(val sc: StringContext) extends AnyVal {

      def re = sc.parts.mkString.re

    }

  }

  /** The TDFA implementation of regular expression. Import this object to use it. */
  object automaton {

    implicit val impl = tdfa.TDfaImpl

    implicit class RegexString(val string: String) extends AnyVal {

      def re: Regex =
        Regex(string)

    }

    implicit class RegexContext(val sc: StringContext) extends AnyVal {

      def re = sc.parts.mkString.re

    }

  }

}

