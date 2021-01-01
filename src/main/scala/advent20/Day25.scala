package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec


/**
 * 🎄
 */
object Day25 extends App {
  val lines = new InputFileReader("input25").getLines
  val cardPublicKey = lines.head.toInt
  val doorPublicKey = lines(1).toInt

  val cardLoopSize = findLoopSize(cardPublicKey)
  val doorLoopSize = findLoopSize(doorPublicKey)

  val encryptionKey = findEncryptionKey(cardLoopSize, doorPublicKey)
  println("Answer " + encryptionKey)

  def findLoopSize(publicKey: BigInt): Int = {
    @tailrec
    def findLoopSize0(value: BigInt, subjectNumber: BigInt, possibleLoopSize: Int): Int = {
      val newValue = transform(value, subjectNumber)
      if (newValue == publicKey) possibleLoopSize
      else findLoopSize0(newValue, subjectNumber, possibleLoopSize + 1)
    }

    findLoopSize0(1, 7, 1)
  }

  def findEncryptionKey(loopSize: Int, publicKey: BigInt): BigInt = {
    @tailrec
    def findEncryptionKey0(loopSize: Int, value: BigInt): BigInt = loopSize match {
      case 0 => value
      case _ => findEncryptionKey0(loopSize - 1, transform(value, publicKey))
    }

    findEncryptionKey0(loopSize, 1)
  }

  private def transform(value: BigInt, subjectNumber: BigInt): BigInt = {
    val divisor = 20201227
    (value * subjectNumber) % divisor
  }
}