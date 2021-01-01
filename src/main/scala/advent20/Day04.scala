package advent20

import advent20.utils.InputFileReader

import scala.collection.mutable.ListBuffer

object Day04 extends App {
  val requiredKeys = Set(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
  )
  val heightRegex = raw"([0-9]+)(cm|in)".r
  val hairRegex = "#[0-9a-f]{6}".r
  val eyeColours = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val pidRegex = "([0-9]){9}".r

  val lines = new InputFileReader("input04").getLines

  private val passports: List[List[String]] = createPassports(lines)
  println("Answer (part 1): " + validPassports(passports))

  val validByValues = passports.filter(p => validPassportKeys(passportKeys(p))).map(passportMap).count(validPassportValues)
  println("Answer (part 2): " + validByValues)

  def validPassports(passports: List[List[String]]) = passports.map(passportKeys).count(validPassportKeys)

  def passportKeys(passport: List[String]) = passport.map(_.split(':')(0)).toSet

  def validPassportKeys(passportKeys: Set[String]) = requiredKeys.diff(passportKeys).isEmpty

  def passportMap(passport: List[String]): Map[String, String] = passport.map(p => {
    val s = p.split(':')
    (s(0), s(1))
  }).toMap

  def validPassportValues(passport: Map[String, String]): Boolean = {
    passport.forall({ case (key, value) => key match {
      case "byr" =>
        value.toIntOption match {
          case Some(year) => year >= 1920 && year <= 2002
          case None => false
        }
      case "iyr" => value.toIntOption match {
        case Some(year) => year >= 2010 && year <= 2020
        case None => false
      }
      case "eyr" => value.toIntOption match {
        case Some(year) => year >= 2020 && year <= 2030
        case None => false
      }
      case "hgt" => value match {
        case heightRegex(height, unit) => unit match {
          case "cm" => height.toInt >= 150 && height.toInt <= 193
          case "in" => height.toInt >= 59 && height.toInt <= 76
          case _ => false
        }
        case _ => false
      }
      case "hcl" => hairRegex.matches(value)
      case "ecl" => eyeColours.contains(value)
      case "pid" => pidRegex.matches(value)
      case _ => true
    }
    })
  }

  def createPassports(lines: List[String]): List[List[String]] = {
    var passportVals = new ListBuffer[List[String]]()
    var currentPassport = new ListBuffer[String]()

    for (line <- lines) {
      if (line.isEmpty) {
        passportVals += currentPassport.toList
        currentPassport = new ListBuffer[String]()
      } else {
        currentPassport ++= line.split(' ').toList
      }
    }

    passportVals.toList
  }
}
