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

class GapBufferTest extends FlatSpec with Matchers {

  "The initial content size" should "be empty" in {
    val gb = new GapBuffer(1024)

    gb.bufferSize should be(1024)
    gb.contentSize should be(0)
    gb.toString should be("")

    gb.cursor should be(0)

  }

  "Inserting text that fits into the gap" should "not modify the buffer size" in {

    val gb = new GapBuffer(10)

    gb.insert('1')
    gb.bufferSize should be(10)
    gb.contentSize should be(1)

    gb.insert("23456789")
    gb.bufferSize should be(10)
    gb.contentSize should be(9)
    gb.toString should be("123456789")

  }

  it should "modify the buffer size if the gap is empty after insertion" in {

    val gb = new GapBuffer(10)

    gb.insert('1')
    gb.bufferSize should be(10)
    gb.contentSize should be(1)

    gb.insert("234567890")
    gb.bufferSize should be(15)
    gb.contentSize should be(10)
    gb.toString should be("1234567890")

  }

  "Inserting a text that does not fit into the gap" should "result in buffer expansion" in {

    val gb = new GapBuffer(10)

    gb.insert("123456789012345678901234567890")

    gb.bufferSize should be(33)
    gb.contentSize should be(30)
    gb.toString should be("123456789012345678901234567890")

  }

  "Cursor" should "initially be at position 0" in {

    val gb = new GapBuffer(10)

    gb.cursor should be(0)

  }

  it should "be moved after the insert text" in {

    val gb = new GapBuffer(10)

    gb.insert('1')
    gb.cursor should be(1)

    gb.insert("2345")
    gb.cursor should be(5)

    gb.insert("67890")
    gb.cursor should be(10)

  }

  it should "be moved by the given delta" in {

    val gb = new GapBuffer(10)
    gb.insert("1234567890")

    gb.moveBy(-5)
    gb.cursor should be(5)

    gb.moveBy(3)
    gb.cursor should be(8)

  }

  it should "be the place where new content is added" in {

    val gb = new GapBuffer(10)
    gb.insert("123456789")

    gb.moveBy(-4)

    gb.insert("youpi")

    gb.cursor should be(10)
    gb.toString should be("12345youpi6789")

  }

}
