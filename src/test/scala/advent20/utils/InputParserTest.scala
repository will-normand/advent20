package advent20.utils

import org.scalatest.flatspec.AnyFlatSpec

class InputParserTest extends AnyFlatSpec {

  behavior of "InputParser"

  it should "groupByBlankLines with an empty list" in {
    assertResult(List(List.empty)) {
      InputParser.groupByBlankLines(List.empty)
    }
  }

  it should "groupByBlankLines with a list with no blank lines" in {
    val input = List("a", "b", "c")
    assertResult(List(input)) {
      InputParser.groupByBlankLines(input)
    }
  }

  it should "groupByBlankLines with a list with blank lines" in {
    val input = List("a", "b", "c", "", "de", "", "f", "gr")
    assertResult(List(List("a", "b", "c"), List("de"), List("f", "gr"))) {
      InputParser.groupByBlankLines(input)
    }
  }
}
