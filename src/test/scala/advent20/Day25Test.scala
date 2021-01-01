package advent20

import advent20.Day25.{findEncryptionKey, findLoopSize}
import org.scalatest.flatspec.AnyFlatSpec

class Day25Test extends AnyFlatSpec {

  behavior of "Day 25"

  it should "findLoopSize for card example" in {
    assertResult(8) {
      findLoopSize(5764801)
    }
  }

  it should "findLoopSize for door example" in {
    assertResult(11) {
      findLoopSize(17807724)
    }
  }

  it should "findEncryptionKey of example" in {
    assertResult(14897079) {
      findEncryptionKey(8, 17807724)
    }
  }
}
