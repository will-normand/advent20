package advent20

import advent20.utils.InputFileReader

import scala.util.matching.Regex

object Day07 extends App {
  val lines = new InputFileReader("input07").getLines
  val bagRules = lines map BagRule.fromString
  println(bagRules)

  private val bagCount: Int = bagColours("shiny gold").toSet.size
  println("Answer (part 1): " + bagCount)

  private val totalBagCount = totalBags("shiny gold")
  println("Answer (part 2): " + totalBagCount)

  def bagColours(colour: String): List[Bag] = {
    val rules = for {
      bagRule: BagRule <- bagRules
      if bagRule.containsColour(colour)
      otherRules: List[Bag] = bagRule.bag :: bagColours(bagRule.bag.colour)
    } yield otherRules
    rules.flatten
  }

  def totalBags(colour: String): Int = bagRules.find(_.bag.colour == colour) match {
    case Some(bagRule) => {
      bagRule.directChildCount + bagRule.contents.map(a => a._1 * totalBags(a._2.colour)).sum
    }
  }
}

case class BagRule(bag: Bag, contents: List[(Int, Bag)]) {
  def containsColour(colour: String): Boolean = contents.exists(_._2.colour == colour)

  def directChildCount: Int = contents.map(_._1).sum
}

case object BagRule {
  def fromString(bagRuleStr: String): BagRule = {
    val bagRulePattern = "(.*) contain (.*).".r
    val quantityPattern = " *([0-9]+) (.*)".r
    bagRuleStr match {
      case bagRulePattern(bagStr, bagsStr) => {
        val bag = Bag.fromString(bagStr)
        val bags = if (bagsStr == "no other bags") {
          Array.empty[(Int, Bag)]
        } else {
          bagsStr.split(',') map {
            case quantityPattern(quantity, bagStr) => (quantity.toInt, Bag.fromString(bagStr))
          }
        }
        BagRule(bag, bags.toList)
      }
    }
  }
}

case class Bag(colour: String)

case object Bag {
  val bagPattern: Regex = "([a-z]+ [a-z]+) bags*".r

  def fromString(bagStr: String): Bag = bagStr match {
    case bagPattern(colour) => Bag(colour)
  }
}