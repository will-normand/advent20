package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec

object Day09 extends App {
  val preambleSize = 25
  val lines = new InputFileReader("input09").getLines map (BigInt(_))

  val invalidNumber = firstInvalid(preambleSize, lines)
  println("Answer (part 1) = " + invalidNumber)

  val weakness = encryptionWeakness(invalidNumber.get, lines)
  println("Answer (part 2) = " + weakness)

  @tailrec
  def firstInvalid(preambleSize: Int, input: List[BigInt]): Option[BigInt] = {
    if (input.size < preambleSize) None
    else {
      val preamble = input.take(preambleSize)
      val next = input(preambleSize)
      if (sumOfTwo(next, preamble)) firstInvalid(preambleSize, input.tail)
      else Some(next)
    }
  }

  @tailrec
  def encryptionWeakness(invalidNumber: BigInt, input: List[BigInt]): BigInt = {
    val weakness = for (i <- 0 to input.size;
                        s = input.take(i)
                        if s.sum == invalidNumber) yield s

    if (weakness.isEmpty) encryptionWeakness(invalidNumber, input.tail)
    else weakness.head.min + weakness.head.max
  }

  def sumOfTwo(a: BigInt, ls: List[BigInt]): Boolean = {
    val numbers = ls.toSet
    ls.exists(b => numbers.contains(a - b))
  }
}
