package advent20

import advent20.utils.InputFileReader

object Day01 extends App {

  val expenses = getInput("input01")

  def productsOfTwo(indexedExpenses: List[(Int, Int)]) =
    for {
      (x, xi) <- indexedExpenses
      (y, yi) <- indexedExpenses
      if xi != yi
      if (x + y) == 2020
    } yield x * y

  println("Answer (part 1): " + productsOfTwo(expenses.zipWithIndex).head)

  def productsOfThree(indexedExpenses: List[(Int, Int)]) =
    for {
      (x, xi) <- indexedExpenses
      (y, yi) <- indexedExpenses
      (z, zi) <- indexedExpenses
      if (x + y + z) == 2020
      if Set(xi, yi, zi).size == 3
    } yield x * y * z

  println("Answer (part 2): " + productsOfThree(expenses.zipWithIndex).head)

  def productsOfN(n: Int, expenses: List[Int]) =
    expenses.combinations(n).filter(_.sum == 2020).toList.head.product

  println("Answer (part 1) from combinations: " + productsOfN(2, expenses))
  println("Answer (part 2) from combinations: " + productsOfN(3, expenses))

  private def getInput(filename: String) =
    new InputFileReader(filename).getLines map (_.toInt)
}
