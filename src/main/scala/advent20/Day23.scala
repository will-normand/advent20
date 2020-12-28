package advent20

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day23 extends App {
  val input = "389125467"
  val cups = ListBuffer.empty ++ input.map(_.toString.toInt)

  println(cups)
  val answer1 = playPart1(cups)
  println("Answer (part 1) " + answer1)

  val answer2 = playPart2(cups)
  println("Answer (part 2) " + answer2)

  def play(cups: ListBuffer[Int], rounds: Int): ListBuffer[Int] = {
    var index = 0
    val lowestLabel = cups.min
    val highestLabel = cups.max
    for (round <- 1 to rounds) {
      if ((round % 100000) == 0) {
        println("round " + round)
      }
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

  def playPart1(cups: ListBuffer[Int]): String = {
    def result(cups: ListBuffer[Int]): String = {
      val indexOfOne = cups.indexOf(1)
      val result = cups.drop(indexOfOne + 1) ++ cups.take(indexOfOne)
      result.map(_.toString).mkString
    }

    val resultCups = play(cups, 100)
    result(resultCups)
  }

  def playPart2(cups: ListBuffer[Int]): BigInt = {
    def result(cups: ListBuffer[Int]): BigInt = {
      val indexOfOne = cups.indexOf(1)

      val results = cups.slice(indexOfOne + 1, indexOfOne + 3)
      println(results)
      println(indexOfOne)
      val cup1 = BigInt(results(0))
      val cup2 = BigInt(results(1))
      val result: BigInt = cup1 * cup2
      println(s"cup1: $cup1, cup2: $cup2, result: $result")
      result
    }

    val extraCups = (cups.max + 1) to 1000000
    val allCups = cups ++ extraCups
    println("All cups size " + allCups.size)
    println(allCups take 20)

    val resultCups = play(allCups, 10000000)
    result(resultCups)
  }
}
