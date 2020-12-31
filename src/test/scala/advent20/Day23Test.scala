package advent20

import advent20.Day23.{playPart1, playPart1Lists, playPart2}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ListBuffer

class Day23Test extends AnyFlatSpec {

  behavior of "Part 1 original"

  it should "work for the example" in {
    assertResult("67384529") {
      val cups = ListBuffer.empty ++ "389125467".map(_.toString.toInt)
      playPart1Lists(cups)
    }
  }

  behavior of "Part 1 faster version"

  it should "work for the example" in {
    assertResult("67384529") {
      val cups = ListBuffer.empty ++ "389125467".map(_.toString.toInt)
      playPart1(cups.toList)
    }
  }

  behavior of "Part 2"

  it should "work for the example" in {
    assertResult(BigInt("149245887792")) {
      val cups = "389125467".map(_.toString.toInt).toList
      playPart2(cups)
    }
  }

  behavior of "Cup"

  it should "create a loop" in {
    assertResult(2) {
      val cups = Cup.createCups(List(1, 2, 3))
      cups.next.next.next.next.label
    }
  }

  it should "work for a big list" in {
    assertResult(5) {
      val cups = Cup.createCupsPadded(List(1, 2, 3), 100000)
      cups.next.next.next.next.label
    }
  }

  behavior of "Cups"

  it should "allow looking up cups" in {
    assertResult(12345 ) {
      val cupRing = Cup.createCupsPadded(List(1, 2, 3), 100000)
      val cups = Cups(cupRing)
      cups.lookup(12344).next.label
    }
  }

  it should "do a move" in {
    val cups = Cups(Cup.createCups(List(1, 2, 3, 4, 5, 6)))
    cups.move()

    assertResult(5)(cups.current.label)
    assertResult(6)(cups.current.next.label)
    assertResult(2)(cups.current.next.next.label)
    assertResult(3)(cups.current.next.next.next.label)
    assertResult(4)(cups.current.next.next.next.next.label)
    assertResult(1)(cups.current.next.next.next.next.next.label)
    assertResult(5)(cups.current.next.next.next.next.next.next.label)
  }

  it should "do a second move" in {
    val cups = Cups(Cup.createCups(List(1, 2, 3, 4, 5, 6)))
    cups.move()
    cups.move()

    assertResult(4)(cups.current.label)
    assertResult(6)(cups.current.next.label)
    assertResult(2)(cups.current.next.next.label)
    assertResult(3)(cups.current.next.next.next.label)
    assertResult(1)(cups.current.next.next.next.next.label)
    assertResult(5)(cups.current.next.next.next.next.next.label)
    assertResult(4)(cups.current.next.next.next.next.next.next.label)
  }
}
