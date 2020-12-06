package advent20.utils

import scala.annotation.tailrec

object InputParser {
  def groupByBlankLines(lines: List[String]): List[List[String]] = {
    @tailrec
    def group(lines: List[String], current: List[String], acc: List[List[String]]): List[List[String]] = lines match {
      case Nil => current :: acc
      case x :: xs =>
        if (x.isEmpty)
          group(xs, List.empty, current :: acc)
        else
          group(xs, x :: current, acc)
    }

    group(lines.reverse, List.empty, List.empty)
  }
}


