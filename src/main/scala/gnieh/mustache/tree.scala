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
package gnieh.mustache

sealed trait Stmt {
  def nonEmpty: Boolean =
    true
}

final case class Var(name: String, escaped: Boolean) extends Stmt

final case class Sec(name: String, content: List[Stmt], inverted: Boolean) extends Stmt

final case class Par(name: String) extends Stmt

case object Com extends Stmt {
  override def nonEmpty =
    false
}

case object SetDelim extends Stmt {
  override def nonEmpty =
    false
}

final case class Txt(content: String) extends Stmt
