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

sealed trait Command {
  def exec(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): (Vector[Int], Map[Long, Int])
}

final case class Assign(store: Variable, value: Value) extends Command {

  def exec(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): (Vector[Int], Map[Long, Int]) =
    store match {
      case Tag(id)         => (tags.updated(id, value.eval(currentPos, tags, maps)), maps)
      case TagMap(id, idx) => (tags, maps.updated(((id << 32) | idx), value.eval(currentPos, tags, maps)))
    }

  override def toString = f"$store ← $value"

}

final case class Reorder(tag: Int, originIndex: Int, targetIndex: Int) extends Command {

  def exec(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): (Vector[Int], Map[Long, Int]) =
    (tags, maps.updated(((tag << 32) | targetIndex), maps(((tag << 32) | originIndex))))

  override def toString = f"m<sub>$tag</sub><sup>$targetIndex</sup> ← m<sub>$tag</sub><sup>$originIndex</sup>"

}

sealed trait Value {
  def eval(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): Int
}

sealed trait Variable extends Value

final case class Tag(id: Int) extends Variable {

  def eval(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): Int =
    tags(id)

  override def toString = f"t<sub>$id</sub>"

}

final case class TagMap(id: Int, index: Int) extends Variable with Comparable[TagMap] {

  def eval(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): Int =
    maps(((id << 32) | index))

  def compareTo(that: TagMap) = {
    val idCmp = this.id - that.id
    if (idCmp == 0) {
      this.index - that.index
    } else {
      idCmp
    }
  }

  override def toString = f"m<sub>$id</sub><sup>$index</sup>"

}

case object CurrentPosition extends Value {

  def eval(currentPos: Int, tags: Vector[Int], maps: Map[Long, Int]): Int =
    currentPos

  override def toString = "<i>p</i>"

}
