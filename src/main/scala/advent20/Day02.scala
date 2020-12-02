package advent20

import advent20.utils.InputFileReader

object Day02 extends App {
  val lines = new InputFileReader("input02").getLines

  val policyPasswords = lines map parseLine

  val validPasswordsCount = policyPasswords.count(pp => pp._1.compliesByCount(pp._2))
  println("Answer (part 1): " + validPasswordsCount)

  val validPasswordsCountPart2 = policyPasswords.count(pp => pp._1.compliesByPosition(pp._2))
  println("Answer (part 2): " + validPasswordsCountPart2)

  private def parseLine(line: String): (PasswordPolicy, String) = {
    val lineParts = line.split(':')
    val policy = PasswordPolicy.fromString(lineParts(0).trim)
    val password = lineParts(1).trim
    (policy, password)
  }
}

case class PasswordPolicy(a: Int, b: Int, char: Char) {
  def compliesByCount(password: String): Boolean = {
    val count = password.count(_ == char)
    a <= count && count <= b
  }

  def compliesByPosition(password: String): Boolean = {
    val matchAtA = password(a - 1) == char
    val matchAtB = password(b - 1) == char
    (matchAtA || matchAtB) && !(matchAtA && matchAtB)
  }
}

case object PasswordPolicy {
  def fromString(code: String): PasswordPolicy = {
    code.split(Array('-', ' ')) match {
      case Array(min, max, char) => PasswordPolicy(min.toInt, max.toInt, char.toCharArray.head)
    }
  }
}
