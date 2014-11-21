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

sealed trait Statement

final case class Variable(name: String, escaped: Boolean) extends Statement

final case class Section(name: String, content: List[Statement], inverted: Boolean) extends Statement

final case class Partial(name: String) extends Statement

final case class Text(content: String) extends Statement
