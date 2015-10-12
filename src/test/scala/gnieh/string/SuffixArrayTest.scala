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
package gnieh.string

import org.scalatest._

class SuffixArrayTest extends FlatSpec with ShouldMatchers {

  "The suffix array of a string consisting of one single letter" should "be the reversed array of indices" in {
    val sa = SuffixArray.suffixArray("ööööööööööööööööööö")
    sa should be(sa.indices.reverse)
  }

  "The suffix array of an alphabetically sorted string" should "be the array of indices" in {
    val sa = SuffixArray.suffixArray("abcdefgh")
    sa should be(sa.indices)
  }

  "The suffix array of an reverse alphabetically sorted string" should "be the reversed array of indices" in {
    val sa = SuffixArray.suffixArray("abcdefgh".reverse)
    sa should be(sa.indices.reverse)
  }

  "The suffix array of mississippi" should "be [10, 7, 4, 1, 0, 9, 8, 6, 3, 5, 2]" in {
    val sa = SuffixArray.suffixArray("mississippi")
    sa should be(Vector(10, 7, 4, 1, 0, 9, 8, 6, 3, 5, 2))
  }

}
