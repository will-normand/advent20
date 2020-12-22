package advent20

import advent20.utils.{InputFileReader, InputParser}

import scala.annotation.tailrec

object Day22 extends App {
  val lines = new InputFileReader("input22").getLines
  val hand1 :: hand2 :: _ = InputParser.groupByBlankLines(lines).map(_.tail).map(hand => hand.map(card => card.toInt))

  val winningHand = game(hand1, hand2)
  val answer1 = calculateScore(winningHand)
  println("Answer (part 1) " + answer1)

  def calculateScore(hand: List[Int]): Int = hand.reverse.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum

  @tailrec
  def game(hand1: List[Int], hand2: List[Int]): List[Int] =
    if (hand1.isEmpty) hand2
    else if (hand2.isEmpty) hand1
    else {
      if (hand1.head > hand2.head) game(hand1.tail :+ hand1.head :+ hand2.head, hand2.tail)
      else game(hand1.tail, hand2.tail :+ hand2.head :+ hand1.head)
    }
}
