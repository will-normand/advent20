package advent20

import advent20.utils.InputFileReader

object Day05 extends App {
  val lines = new InputFileReader("input05").getLines
  private val seatCodes = lines.map(SeatParser.calculateSeatCode)
  val answer = seatCodes.max
  println("Answer (part 1) is " + answer)

  val sortedCodes = seatCodes.sorted
  val pairs = sortedCodes.zip(sortedCodes.tail)
  val mySeat = pairs.find({ case (a, b) => a + 1 != b }) match { case Some(p) => p._1 + 1 }
  println("Answer (part 2) is " + mySeat)
}

object SeatParser {
  val seat: String => (Int, Int) = line => split(line) match {
    case (rowCode, seatCode) => (parseRow(rowCode), parseSeat(seatCode))
  }
  val seatCode: ((Int, Int)) => Int = {
    case (row, seat) => row * 8 + seat
  }

  def calculateSeatCode(line: String): Int = {
    seatCode(seat(line))
  }

  def split(line: String): (String, String) = {
    line.splitAt(7)
  }

  def parseRow(rowCode: String): Int = parseCode(rowCode, 'B')

  def parseSeat(seatCode: String): Int = parseCode(seatCode, 'R')

  def parseCode(rowCode: String, one: Char): Int = {
    var row: Int = 0
    rowCode.foreach((char: Char) => {
      val bin = if (char == one) 1 else 0
      row <<= 1
      row |= bin
    })
    row
  }
}
