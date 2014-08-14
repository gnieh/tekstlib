/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package hyphen

class StringTrie[+T] private(val value: Option[T], children: Map[Char, StringTrie[T]]) {

  def get(key: String): Option[T] = key match {
    case null | "" =>
      value
    case k !:: ks =>
      for {
        child <- children.get(k)
        v <- child.get(ks)
      } yield v

  }

  def updated[U >: T](key: String, value: U): StringTrie[U] = key match {
    case null | "" =>
      new StringTrie(Some(value), children)

    case k !:: "" =>
      new StringTrie(this.value, children.updated(k, new StringTrie(Some(value), Map())))

    case k !:: ks =>
      children.get(k) match {
        case Some(child) =>
          new StringTrie(this.value, children.updated(k, child.updated(ks, value)))
        case None =>
          new StringTrie(this.value, children.updated(k, fromString(ks, value)))
      }
  }

  def derive(c: Char): StringTrie[T] =
    children.get(c) match {
      case Some(trie) => trie
      case None       => StringTrie.Empty
    }

  def contains(key: String): Boolean = key match {
    case null | "" =>
      value.isDefined
    case k !:: ks =>
      children.get(k).fold(false)(_.contains(ks))
  }

  def derivableAt(c: Char): Boolean =
    children.contains(c)

  /* rapidly create a single-value trie from a string */
  private def fromString[T](key: String, value: T): StringTrie[T] =
    key.foldRight(new StringTrie(Some(value), Map())) { (c, acc) =>
      new StringTrie(None, Map(c -> acc))
    }

  override def toString = {
    val v = value match {
      case Some(v) => s"$v, "
      case None    => ""
    }
    s"StringTrie($v$children)"
  }

}

object StringTrie {

  def empty[T](): StringTrie[T] =
    new StringTrie(None, Map())

  val Empty = empty

}
