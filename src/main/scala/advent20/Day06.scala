package advent20

import advent20.utils.{InputFileReader, InputParser}

object Day06 extends App {
  val lines = new InputFileReader("input06")
  val grouped = InputParser.groupByBlankLines(lines.getLines)
  println(grouped)

  val part1 = sumOfYesCounts(grouped)
  println("Answer (part 1) " + part1)

  val part2 = sumOfEveryYesCounts(grouped)
  println("Answer (part 2) " + part2)

  def sumOfYesCounts(input: List[List[String]]): Int = (input map (_.reduce((a, b) => a + b)) map (_.toSet.size)).sum

  def sumOfEveryYesCounts(input: List[List[String]]): Int = (input map everyoneYes).sum

  def everyoneYes(groupInput: List[String]): Int = (groupInput map (_.toSet) reduce ((a, b) => a & b)).size
}
