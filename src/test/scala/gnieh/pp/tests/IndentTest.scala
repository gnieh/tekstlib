package gnieh.pp.tests

import gnieh.pp._

class IndentTest extends PpTest {
  "the indent operator" should "indent text as block" in {
    val docs: List[Doc] = List("line1", "line2", "line3")
    val finalDoc = indent(2)(vcat(docs))

    render80(finalDoc) should be("  line1\n  line2\n  line3")
  }
  it should "behave as original Haskell implementation" in {
    val doc = indent(4)(fillSep(words("the indent combinator indents these words !")))

    val expectedRender =
      """    the indent
        |    combinator
        |    indents these
        |    words !""".stripMargin

    render20(doc) should be(expectedRender)
  }
}
