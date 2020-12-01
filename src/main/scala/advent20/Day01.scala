package advent20

import scala.io.Source

object Day01 extends App {

  val expenses = getInput("input01.txt").zipWithIndex

  def productsOfTwo =
    for {
      (x, xi) <- expenses
      (y, yi) <- expenses
      if xi != yi
      if (x + y) == 2020
    } yield x * y

  println("Answer (part 1): " + productsOfTwo.head)

  def productsOfThree =
    for {
      (x, xi) <- expenses
      (y, yi) <- expenses
      (z, zi) <- expenses
      if (x + y + z) == 2020
      if Set(xi, yi, zi).size == 3
    } yield x * y * z

  println("Answer (part 2): " + productsOfThree.head)

  private def getInput(filename: String) = readFile(filename) map { _.toInt }

  private def readFile(filename: String): List[String] = {
    val source = Source.fromResource(filename)
    val lines = source.getLines().toList
    source.close()
    lines
  }
}
