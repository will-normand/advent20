package advent20

import advent20.utils.{InputFileReader, InputParser}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day22 extends App {
  val lines = new InputFileReader("input22").getLines
  val hand1 :: hand2 :: _ = parseHands(lines)

  def parseHands(lines: List[String]): List[List[Int]] = {
    InputParser.groupByBlankLines(lines).map(_.tail).map(hand => hand.map(card => card.toInt))
  }

  val winningHand = combat(hand1, hand2)
  val answer1 = calculateScore(winningHand)
  println("Answer (part 1) " + answer1)

  val winningHand2 = recursiveCombat(hand1, hand2)
  val answer2 = calculateScore(winningHand2)
  println("Answer (part 2) " + answer2)

  def calculateScore(hand: List[Int]): Int = hand.reverse.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum

  @tailrec
  def combat(hand1: List[Int], hand2: List[Int]): List[Int] =
    if (hand1.isEmpty) hand2
    else if (hand2.isEmpty) hand1
    else {
      if (hand1.head > hand2.head) combat(hand1.tail :+ hand1.head :+ hand2.head, hand2.tail)
      else combat(hand1.tail, hand2.tail :+ hand2.head :+ hand1.head)
    }

  def recursiveCombat(hand1: List[Int], hand2: List[Int]): List[Int] = {

    @tailrec
    def recursiveCombatWithHistory(hand1: Queue[Int], hand2: Queue[Int], history: Set[(Queue[Int], Queue[Int])]): (Int, Queue[Int]) =
      if (hand1.isEmpty) (2, hand2)
      else if (hand2.isEmpty) (1, hand1)
      else if (history.contains((hand1, hand2))) (1, hand1)
      else if (canRecurse(hand1) && canRecurse(hand2)) {
        val result = goDeeper(hand1.tail.take(hand1.head), hand2.tail.take(hand2.head))
        if (result._1 == 1) recursiveCombatWithHistory(hand1.tail.enqueueAll(List(hand1.head, hand2.head)), hand2.tail, history + Tuple2(hand1, hand2))
        else recursiveCombatWithHistory(hand1.tail, hand2.tail.enqueueAll(List(hand2.head, hand1.head)), history + Tuple2(hand1, hand2))
      }
      else {
        if (hand1.head > hand2.head) recursiveCombatWithHistory(hand1.tail.enqueueAll(List(hand1.head, hand2.head)), hand2.tail, history + Tuple2(hand1, hand2))
        else recursiveCombatWithHistory(hand1.tail, hand2.tail.enqueueAll(List(hand2.head, hand1.head)), history + Tuple2(hand1, hand2))
      }

    def goDeeper(hand1: Queue[Int], hand2: Queue[Int]): (Int, Queue[Int]) =
      recursiveCombatWithHistory(hand1, hand2, Set.empty)

    def canRecurse(hand: Queue[Int]): Boolean = hand.tail.size >= hand.head

    recursiveCombatWithHistory(Queue.empty.enqueueAll(hand1), Queue.empty.enqueueAll(hand2), Set.empty)._2.toList
  }
}
