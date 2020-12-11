package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day10 extends App {
  val lines = new InputFileReader("input10").getLines
  val adaptorJoltages = lines.map(_.toInt).sorted
  val joltages = 0 :: (adaptorJoltages :+ adaptorJoltages.last + 3)
  val joltageDiffs = diffs(joltages)
  println("Answer (part 1) " + oneJoltsTimeThreeJolts(joltageDiffs))

  val arrangements = arrangementsCount(joltageDiffs)
  println(f"Answer (part 2) $arrangements%f")

  def diffs(joltages: List[Int]): List[Int] = {
    @tailrec
    def diffs0(joltages: List[Int], acc: List[Int]): List[Int] = {
      joltages match {
        case first :: second :: rest => diffs0(second :: rest, (second - first) :: acc)
        case _ :: Nil => acc
      }
    }

    diffs0(joltages, List.empty)
  }

  def oneJoltsTimeThreeJolts(joltageDiffs: List[Int]): Int = joltageDiffs.count(_ == 1) * joltageDiffs.count(_ == 3)
  
  def arrangementsCount(joltageDiffs: List[Int]): Double = {
    val seqs = countSeqs(joltageDiffs.toVector)
    val factors = List(math.pow(2, seqs(2)), math.pow(4, seqs(3)), math.pow(7, seqs(4)))
    factors.product
  }

  def countSeqs(joltageDiffs: Vector[Int]): Predef.Map[Int, Int] = {
    // This is ugly
    var i = 0
    val result: mutable.Map[Int, Int] = mutable.Map()
    var contiguous = ArrayBuffer[Int]()
    while (i < joltageDiffs.length) {
      val e = joltageDiffs(i)
      if (e == 1) {
        contiguous += e
      } else {
        val current = result.getOrElse(contiguous.length, 0)
        result.put(contiguous.length, current + 1)
        contiguous = ArrayBuffer()
      }
      i += 1
    }
    val current = result.getOrElse(contiguous.length, 0)
    result.put(contiguous.length, current + 1)

    result.toMap
  }

  def countSlice(seq: Seq[Int], slice: Seq[Int]): Int = {
    var count = 0
    var index = seq.indexOfSlice(slice)
    while (index > 0) {
      count += 1
      index = seq.indexOfSlice(slice, index + 1)
    }
    count
  }

  /**
   * Complex (and incorrect) attempt at calculating all lists.
   *
   * Also not tail recursive.
   */
  def arrangements(joltageDiffs: List[Int]): Int = {
    def arrangements0(xs: List[Int], ys: List[Int], acc: List[List[Int]], depth: Int): List[List[Int]] = {
      xs match {
        case 1 :: 1 :: xs => {
          val rest: List[List[Int]] = arrangements0(1 :: xs, ys :+ 1, acc, depth)
          val newList: List[Int] = ys ++ (1 :: xs)
          arrangements0(newList, List.empty, acc ++ rest :+ newList, depth + 1)
        }
        case a :: b :: xs => arrangements0(b :: xs, ys :+ a, acc, depth)
        case _ :: Nil => acc
      }
    }

    val x = arrangements0(joltageDiffs.reverse, List.empty, List(joltageDiffs.reverse), 0)
    x.size
  }
}
