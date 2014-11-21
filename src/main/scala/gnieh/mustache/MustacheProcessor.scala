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

import scala.reflect._

/** A mustache processor is able to process templates from several sources
 *  from classpath or filesystem, or even programmatically built.
 *
 *  This processor does not presume any file extension for templates. You
 *  can load files with any extension and it assumes it is a mustache template file.
 *  So the simple case where you say "load my template file named `my-template.txt` in
 *  directory `dir`" simply works as you expect it to work. No magic there to add
 *  implicit file extension, no complex classpath lookup loading.
 *  You can actually implement the loading strategy of your choice by implementing
 *  [[gnieh.mustache.MustacheLoader]].
 *  By default we provide simple strategies that lookup for files in a repository or in
 *  the classpath as well as a composition stratety.
 *
 *  If the processor is said to be resident, it caches the
 *  templates for more efficient rendering later on.
 *  Cache is never automatically invalidated, this is a simple processor.
 *  You can either invalidate a specific template manually, or clean the all cache
 *  at once.
 *  You can also reload the cache, which checks for each template whether the template
 *  source has changed since we loaded it, and reloads this template in such a case.
 *
 *  @author Lucas Satabin
 */
class MustacheProcessor(loader: MustacheLoader, resident: Boolean = false) {

  private val cache = scala.collection.mutable.Map.empty[String, CachedTemplate]

  private class CachedTemplate(val lastLoaded: Long, val instructions: List[Statement])

  def load(name: String): List[Statement] =
    if(resident && cache.contains(name)) {
      cache(name).instructions
    } else {
      loader.load(name) match {
        case Some(instructions) =>
          if(resident) {
            cache(name) = new CachedTemplate(System.currentTimeMillis, instructions)
          }
          instructions
        case None =>
          throw new Exception(s"Unknown template $name")
      }
    }

  def render(name: String, values: Map[String, Any]): String =
    render(load(name), values).toString

  private def render(instrs: List[Statement], value: Map[String, Any]): StringBuilder =
    instrs.foldLeft(new StringBuilder) { (acc, instr) =>
      instr match {
        case Variable(name, escaped) =>
          renderVar(acc, name, escaped, value)
        case Section(name, content, inverted) =>
          renderSection(acc, name, content, inverted, value)
        case Text(txt) =>
          acc.append(txt)
        case Partial(name) =>
          acc.append(render(load(name), value))
      }
    }

  private def escape(str: String): StringBuilder =
    str.foldLeft(new StringBuilder) { (acc, c) =>
      acc.append(
        c match {
          case ''' => "&apos;"
          case '"' => "&quot;"
          case '&' => "&amp;"
          case '<' => "&lt;"
          case '>' => "&gt;"
          case _   => c
      })
    }

  private def renderVar(acc: StringBuilder, name: String, escaped: Boolean, value: Map[String, Any]): StringBuilder =
    if(value.contains(name))
      if(escaped)
        acc.append(escape(value(name).toString))
      else
        acc.append(value(name))
    else
      acc

  private def renderSection(acc: StringBuilder,
    name: String,
    content: List[Statement],
    inverted: Boolean,
    value: Map[String, Any])(
      implicit mtag: ClassTag[Map[String, Any]],
      mtagl: ClassTag[Seq[Map[String, Any]]],
      ftag: ClassTag[(List[Statement], Map[String, Any], (List[Statement], Map[String, Any]) => StringBuilder) => StringBuilder]): StringBuilder =
    value.get(name) match {
      case Some(false) | Some(Seq()) | Some(null) | None if inverted =>
        // it is undefined or false of empty and inverted, render content
        acc.append(render(content, value))
      case _ if inverted =>
        // otherwise do not render content
        acc
      case Some(true) =>
        // it is true and not inverted, render content
        acc.append(render(content, value))
      case Some(mtag(subValue)) if subValue.nonEmpty =>
        // it is a non empty map, render content with sub-values
        acc.append(render(content, subValue))
      case Some(mtagl(subValues)) if subValues.nonEmpty =>
        // it is a non empty list of maps, render content with sub-values
        subValues.foldLeft(acc) { (acc, map) => acc.append(render(content, map)) }
      case Some(ftag(fun)) =>
        acc.append(fun(content, value, render))
      case _ =>
        acc
    }

}
