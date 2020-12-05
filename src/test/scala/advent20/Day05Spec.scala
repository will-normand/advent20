package advent20

import org.scalatest.flatspec.AnyFlatSpec

class Day05Spec extends AnyFlatSpec {
  "parseRow" should "return 0 for all Fs" in {
    assert(SeatParser.parseRow("FFF").equals(0))
  }

  it should "return 1 for one B" in {
    assertResult(1)(SeatParser.parseRow("B"))
  }

  it should "return 5 for one BFB" in {
    assertResult(5)(SeatParser.parseRow("BFB"))
  }

  it should "return 44 for FBFBBFF" in {
    assertResult(44)(SeatParser.parseRow("FBFBBFF"))
  }

  "seatID" should "match examples" in {
    assertResult(567)(SeatParser.calculateSeatCode("BFFFBBFRRR"))
    assertResult(119)(SeatParser.calculateSeatCode("FFFBBBFRRR"))
    assertResult(820)(SeatParser.calculateSeatCode("BBFFBBFRLL"))
  }
}
