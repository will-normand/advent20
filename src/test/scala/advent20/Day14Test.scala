package advent20

import advent20.Day14.applyMask
import org.scalatest.flatspec.AnyFlatSpec

class Day14Test extends AnyFlatSpec {

  behavior of "Instruction Group"
  it should "generate all 4 floatingMasks" in {
    assertResult(Set(26L, 27L, 58L, 59L)) {
      val mask = "000000000000000000000000000000X1001X"
      val result = applyMask(42, mask)
      result.toSet
    }
  }
  it should "generate all 8 floatingMasks" in {
    assertResult(Set(16L, 17L, 18L, 19L, 24L, 25L, 26L, 27L)) {
      val mask = "00000000000000000000000000000000X0XX"
      val result = applyMask(26, mask)
      result.toSet
    }
  }
}
