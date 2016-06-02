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
package gnieh.regex
package vm

import compiler._
import util._

/** This class provides a way to create and use regular expressions. It is a non backtracking implementation
 *  based on the descrition from [Russ Cox](http://swtch.com/~rsc/regexp/).
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
 *   - `x*?` zero or more `x` (prefer zero)
 *   - `x+?` one or more `x` (prefer one)
 *   - `x??` zero or one `x` (prefer zero)
 *   - `(re)` numbered capturing group (starting at 1)
 *   - `re{min,max}` bounded repetition (`min` and `max` are integers)
 *   - `re{min}` bounded repetition (`min` is an integer)
 *   - `^` start of input anchor
 *   - `$` end of input anchor
 *
 *  @author Lucas Satabin
 */
object BytecodeImpl extends RegexImpl with Serializable {

  type Compiled = Vector[Inst]

  def compile(re: ReNode) = Compiler.compile(re)

  def exec(compiled: Vector[Inst], nbSaved: Int, startIdx: Int, string: String): (Int, Int, Vector[Int]) =
    VM.exec(compiled, nbSaved, startIdx, string)

}
