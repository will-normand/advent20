package advent20

import advent20.utils.InputFileReader

object Day12 extends App {
  val lines = new InputFileReader("input12").getLines

  val initialLocation = Location(0, 0, E)
  val finalPostition = lines.foldLeft(initialLocation)((location, moveDef) => location.move(moveDef))
  println("Answer (part 1) " + finalPostition.manhattan)
}

case class Location(east: Int, north: Int, facing: Direction) {
  private val regex = "([A-Z])([0-9]+)".r
  def move(moveDef: String) : Location = {
    moveDef match {
      case regex(action, valueString) => {
        val value = valueString.toInt
        action match {
          case "N" => Location(east, north + value, facing)
          case "S" => Location(east, north - value, facing)
          case "E" => Location(east + value, north, facing)
          case "W" => Location(east- value, north, facing)
          case "L" => Location(east, north, facing.rotate(action, value))
          case "R" => Location(east, north, facing.rotate(action, value))
          case "F" => move(facing.toString + valueString)
        }
      }
    }
  }

  def manhattan: Int = east.abs + north.abs
}

sealed trait Direction {
  def rotate(turnDirection: String, degrees: Int): Direction = {
    val directions = IndexedSeq(E, S, W, N)
    val turnUnits = if (turnDirection == "R") degrees / 90 else (degrees / 90) * -1
    val currentIndex = directions.indexOf(this)
    val newIndex = Math.floorMod((currentIndex + turnUnits), directions.length)
    directions(newIndex)
  }
}
case object N extends Direction
case object S extends Direction
case object E extends Direction
case object W extends Direction
