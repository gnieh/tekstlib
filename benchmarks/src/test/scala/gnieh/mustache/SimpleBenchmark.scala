/*
* This file is part of the regex project.
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
package gnieh.mustache

import org.scalameter._

import java.io.File

class SimpleBenchmark extends PerformanceTest.Quickbenchmark {

  val template = """Hello {{name}},
                   |This is a simple template featuring several stuffs:
                   |{{#features}}
                   | - {{name}}
                   |{{/features}}
                   |I guess this {{#fun}}works for you, {{name}}{{/fun}}.""".stripMargin

  val templateGen = Gen.single("template")(template)

  val compiledGen = Gen.single("compiled")(new MustacheParser(template).run().get)

  val processor = new MustacheProcessor(new FileSystemLoader(new File(".")), false)

  def subRender(stmts: List[Statement], values: Map[String, Any], render: (List[Statement], Map[String, Any]) => String): String =
    render(stmts, values)

  val values = Map(
    "name" -> "Gérard Lambert",
    "features" -> List(
      Map("name" -> "variables"),
      Map("name" -> "lists"),
      Map("name" -> "functions")),
    "fun" -> subRender _)

  performance of "using precompiled template" in {
    measure method "render" in {
      using(compiledGen) in { t =>
        processor.render(t, values)
      }
    }
  }

  performance of "parsing each time" in {
    measure method "renderString" in {
      using(templateGen) in { t =>
        processor.renderString(t, values)
      }
    }
  }

}

