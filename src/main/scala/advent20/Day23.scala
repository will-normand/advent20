package advent20

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day23 extends App {
  val input = "315679824"
  val cups = ListBuffer.empty ++ input.map(_.toString.toInt)

  println(cups)
  val answer1 = play(cups)
  println("Answer (part 1) " + answer1)

  def play(cups: ListBuffer[Int]): String = {
    var index = 0
    val lowestLabel = cups.min
    val highestLabel = cups.max
    for (_ <- 1 to 100) {
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

    def result(cups: ListBuffer[Int]): String = {
      val indexOfOne = cups.indexOf(1)
      val result = cups.drop(indexOfOne + 1) ++ cups.take(indexOfOne)
      result.map(_.toString).mkString
    }

    result(cups)
  }
}
