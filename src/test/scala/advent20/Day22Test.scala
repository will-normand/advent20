package advent20

import advent20.Day22.{calculateScore, parseHands, recursiveCombat}
import advent20.utils.InputFileReader
import org.scalatest.flatspec.AnyFlatSpec

class Day22Test extends AnyFlatSpec {

  behavior of "recursiveCombat"

  it should "end due to repeated condition" in {
    val lines = new InputFileReader("input22testinf").getLines
    val hand1 :: hand2 :: _ = parseHands(lines)
    val _ = recursiveCombat(hand1, hand2)
  }

  it should "pass test example" in {
    assertResult(291) {
      val lines = new InputFileReader("input22test").getLines
      val hand1 :: hand2 :: _ = parseHands(lines)
      val winningHand = recursiveCombat(hand1, hand2)
      calculateScore(winningHand)
    }
  }
}
