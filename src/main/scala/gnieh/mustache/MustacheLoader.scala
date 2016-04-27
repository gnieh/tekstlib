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

import java.io.File

import scala.io.{
  Source,
  Codec
}

import scala.util.Try

import scala.annotation.tailrec

/** A mustache loader defines the strategy to load mustache files
 *
 *  @author Lucas Satabin
 */
abstract class MustacheLoader {

  /** Loads the template with the given name.
   *  If it is not found, returns `None`
   */
  def load(name: String): Option[List[Statement]]

  protected def fromString(source: String): Try[List[Statement]] = {
    val parser = new MustacheParser(source)
    parser.run()
  }

}

object MustacheLoader {

  def fromDirectory(dir: File): MustacheLoader =
    new FileSystemLoader(dir)

  def fromClassLoader(cl: ClassLoader): MustacheLoader =
    new ClassLoaderLoader(cl)

  def firstOf(loaders: MustacheLoader*): MustacheLoader =
    new SequenceLoader(loaders)

}

/** A simple loader that loads templates from a base directory on the
 *  file system.
 *  Template can be in sub directories in which case the relative path from the
 *  base directory must be given.
 *
 *  @author Lucas Satabin
 */
class FileSystemLoader(base: File) extends MustacheLoader {

  def load(name: String): Option[List[Statement]] = {
    val file = new File(base, name)
    if (file.exists) {
      Option(fromString(Source.fromFile(file).mkString).get)
    } else {
      None
    }
  }

}

class ClassLoaderLoader(cl: ClassLoader) extends MustacheLoader {

  def load(name: String): Option[List[Statement]] = {
    val resource = cl.getResource(name)
    if (resource != null) {
      Option(fromString(Source.fromURL(resource).mkString).get)
    } else {
      None
    }
  }

}

class SequenceLoader(loaders: Seq[MustacheLoader]) extends MustacheLoader {

  def load(name: String): Option[List[Statement]] = {
    @tailrec
    def loop(loaders: Seq[MustacheLoader]): Option[List[Statement]] =
      if (loaders.isEmpty) {
        None
      } else {
        loaders.head.load(name) match {
          case stmts @ Some(_) => stmts
          case None            => loop(loaders.tail)
        }
      }
    loop(loaders)
  }
}
