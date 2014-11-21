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

import org.parboiled2.Parser.DeliveryScheme.Throw

import scala.annotation.tailrec

/** A mustache loader defines the strategy to load mustache files
 *
 *  @author Lucas Satabin
 */
abstract class MustacheLoader {

  /** Loads the template with the given name.
   *  If it is not found, returns `None` */
  def load(name: String): Option[List[Stmt]]

  protected def fromSource(source: Source) = {
    val parser = new MustacheParser(source.mkString)
    parser.MustacheFile.run()
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

  def load(name: String): Option[List[Stmt]] = {
    val file = new File(base, name)
    if(file.exists) {
      Option(fromSource(Source.fromFile(file)))
    } else {
      None
    }
  }

}

class ClassLoaderLoader(cl: ClassLoader) extends MustacheLoader {

  def load(name: String): Option[List[Stmt]] = {
    val resource = cl.getResource(name)
    if(resource != null) {
      Option(fromSource(Source.fromURL(resource)))
    } else {
      None
    }
  }

}

class SequenceLoader(loaders: Seq[MustacheLoader]) extends MustacheLoader {

  def load(name: String): Option[List[Stmt]] = {
    @tailrec
    def loop(loaders: Seq[MustacheLoader]): Option[List[Stmt]] =
      if(loaders.isEmpty) {
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
