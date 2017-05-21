package gnieh.diff
package test

import org.scalatest._

abstract class TestLcs extends FlatSpec with Matchers {

  val lcsImpl: Lcs[Char]

  "the lcs of an empty sequence and another sequence" should "be the empty sequence" in {

    val lcs1 = lcsImpl.lcs("", "abcdef")

    lcs1 should be(Nil)

    val lcs2 = lcsImpl.lcs("abcdef", "")

    lcs2 should be(Nil)

  }

  "the lcs of two equal strings" should "be the strings" in {

    val str = "abcde"
    val lcs = lcsImpl.lcs(str, str)
    lcs should be(List(Common(0, 0, 5)))

  }

  "the lcs of a string and a prefix" should "be the prefix" in {

    val str = "abcdef"
    val prefix = "abc"
    val lcs1 = lcsImpl.lcs(str, prefix)

    lcs1 should be(List(Common(0, 0, 3)))

    val lcs2 = lcsImpl.lcs(prefix, str)

    lcs2 should be(List(Common(0, 0, 3)))

  }

  "the lcs of two strings with no common characters " should "be empty" in {

    val str1 = "abcdef"
    val str2 = "ghijkl"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(Nil)
  }

  "the lcs of two strings" should "be correctly computed when one is in the middle of the other one" in {

    val str1 = "abcdefgh"
    val str2 = "bdeg"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(1, 0, 1), Common(3, 1, 2), Common(6, 3, 1)))
  }

  it should "be correctly computed with a repeated character in common" in {

    val str1 = "abcbdbebf"
    val str2 = "bbbb"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(1, 0, 1), Common(3, 1, 1), Common(5, 2, 1), Common(7, 3, 1)))
  }

  it should "be correctly computed with non unique characters" in {

    val str1 = "abcdabcd"
    val str2 = "gabhakbf"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(0, 1, 2), Common(4, 4, 1), Common(5, 6, 1)))
  }

  it should "be correctly computed when both sequences have a common prefix and suffix" in {

    val str1 = "abctotodef"
    val str2 = "abctatatadef"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(0, 0, 4), Common(5, 5, 1), Common(7, 9, 3)))

  }

  it should "not include an empty common element" in {

    val str1 = "aabbcc"
    val str2 = "aacc"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(0, 0, 2), Common(4, 2, 2)))

  }

  it should "not include overlapping prefix and suffix twice" in {

    val str1 = "aabaa"
    val str2 = "aaa"

    val lcs = lcsImpl.lcs(str1, str2)

    lcs should be(List(Common(0, 0, 2), Common(4, 2, 1)))

  }

}
