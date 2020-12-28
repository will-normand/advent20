package advent20

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day23 extends App {
  val input = "315679824"
  val cups = ListBuffer.empty ++ input.map(_.toString.toInt)

  println(cups)
  val answer1 = playPart1(cups.toList)
  println("Answer (part 1) " + answer1)

  val answer2 = playPart2(cups.toList)
  println("Answer (part 2) " + answer2)

  def play(cups: ListBuffer[Int], rounds: Int): ListBuffer[Int] = {
    var index = 0
    val lowestLabel = cups.min
    val highestLabel = cups.max
    for (round <- 1 to rounds) {
      val currentLabel = cups(index)
      val removed = removeThree(cups, index)
      val destination = destinationCupIndex(cups, currentLabel - 1)
      cups.insertAll(destination + 1, removed)
      index = cups.indexOf(currentLabel)
      index = (index + 1) % cups.size
    }

    @tailrec
    def destinationCupIndex(cups: ListBuffer[Int], label: Int): Int =
      if (label < lowestLabel)
        destinationCupIndex(cups, highestLabel)
      else if (cups.contains(label))
        cups.indexOf(label)
      else
        destinationCupIndex(cups, label - 1)

    def removeThree(cups: ListBuffer[Int], index: Int): ListBuffer[Int] = {
      var removed = cups.slice(index + 1, index + 4)
      cups.remove(index + 1, removed.size)
      if (removed.size < 3) {
        val needToRemove = 3 - removed.size
        removed ++= cups.slice(0, needToRemove)
        cups.remove(0, needToRemove)
      }
      removed
    }

    cups
  }

  def playPart1Lists(cups: ListBuffer[Int]): String = {
    def result(cups: ListBuffer[Int]): String = {
      val indexOfOne = cups.indexOf(1)
      val result = cups.drop(indexOfOne + 1) ++ cups.take(indexOfOne)
      result.map(_.toString).mkString
    }

    val resultCups = play(cups, 100)
    result(resultCups)
  }

  def playPart1(cupLabels: List[Int]): String = {
    def result(cups: Cups): String = cups.labelsFromOne

    val cupRing = Cup.createCups(cupLabels)
    val cups = Cups(cupRing)
    for (_ <- 1 to 100) { cups.move() }
    result(cups)
  }

  def playPart2(cupLabels: List[Int]): BigInt = {
    def result(cups: Cups): BigInt = {
      val oneCup = cups.lookup(1)

      val nextCup = BigInt(oneCup.next.get.label)
      val nextNextCup = BigInt(oneCup.next.get.next.get.label)
      val result: BigInt = nextCup * nextNextCup
      result
    }

    val cupRing = Cup.createCupsPadded(cupLabels, 1000000)
    val cups = Cups(cupRing)
    for (_ <- 1 to 10000000) { cups.move() }
    result(cups)
  }
}

class Cups(var current: Cup, lookupTable: Map[Int, Cup], minLabel: Int, maxLabel: Int) {
  def lookup(label: Int): Cup = lookupTable(label)

  def move(): Unit = {
    val movingFirst = current.next.get
    val movingLast = current.next3
    current.next = movingLast.next
    movingLast.next = None
    val movingLabels = Set(current.label, movingFirst.label, movingFirst.next.get.label, movingLast.label)
    var nextLabel = decrementLabel(current.label)
    while (movingLabels.contains(nextLabel)) {
      nextLabel = decrementLabel(nextLabel)
    }
    val nextCup = lookup(nextLabel)
    val afterInsert = nextCup.next.get
    nextCup.next = Some(movingFirst)
    movingLast.next = Some(afterInsert)
    current = current.next.get
  }

  def labelsFromOne: String = {
    val oneCup = lookup(1)
    var cup = oneCup.next.get

    val labels: ListBuffer[Int] = ListBuffer.empty
    while (cup != oneCup) {
      labels append cup.label
      cup = cup.next.get
    }
    labels.map(_.toString).mkString
  }

  private def decrementLabel(label: Int): Int = if (label - 1 < minLabel) maxLabel else label - 1
}

object Cups {
  def apply(current: Cup): Cups = {
    var lookup: Map[Int, Cup] = Map(current.label -> current)
    var cup = current.next.get
    var minLabel = Int.MaxValue
    var maxLabel = 0
    while (cup != current) {
      lookup = lookup + (cup.label -> cup)
      if (cup.label < minLabel) minLabel = cup.label
      if (cup.label > maxLabel) maxLabel = cup.label
      cup = cup.next.get
    }

    new Cups(current, lookup, minLabel, maxLabel)
  }
}

case class Cup(label: Int, var next: Option[Cup]) {
  def next3: Cup = next.get.next.get.next.get
  override def toString: String = s"Cup($label, next=${next.get.label})"
}

case object Cup {
  def createCups(cups: List[Int]): Cup = {
    @tailrec
    def createCups0(cups: List[Int], prevCup: Cup, firstCup: Cup): Cup = {
      cups match {
        case x :: xs => createCups0(xs, new Cup(x, Some(prevCup)), firstCup)
        case Nil =>
          firstCup.next = Some(prevCup)
          prevCup
      }
    }

    val cupsReversed = cups.reverse
    val firstCup = new Cup(cupsReversed.head, Option.empty)
    createCups0(cupsReversed.tail, firstCup, firstCup)
  }

  def createCupsPadded(cups: List[Int], padding: Int): Cup = {
    val extraCups = (cups.max + 1) to padding
    val allCups = cups ++ extraCups
    assert(allCups.size == padding)
    createCups(allCups)
  }
}
