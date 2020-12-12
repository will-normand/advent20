package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec

object Day12 extends App {
  val lines = new InputFileReader("input12").getLines

  val initialLocation = Location(0, 0, E)
  val finalPostition = lines.foldLeft(initialLocation)((location, moveDef) => location.move(moveDef))
  println("Answer (part 1) " + finalPostition.manhattan)

  val initialWaypointLocation = WaypointLocation((0, 0), (10, 1))
  val finalWaypointPostition = lines.foldLeft(initialWaypointLocation)((location, moveDef) => location.move(moveDef))
  println("Answer (part 2) " + finalWaypointPostition.manhattan)
}

case class Location(east: Int, north: Int, facing: Direction) {
  private val regex = "([A-Z])([0-9]+)".r

  def move(moveDef: String): Location = moveDef match {
    case regex(action, valueString) => {
      val value = valueString.toInt
      action match {
        case "N" => Location(east, north + value, facing)
        case "S" => Location(east, north - value, facing)
        case "E" => Location(east + value, north, facing)
        case "W" => Location(east - value, north, facing)
        case "L" => Location(east, north, facing.rotate(action, value))
        case "R" => Location(east, north, facing.rotate(action, value))
        case "F" => move(facing.toString + valueString)
      }
    }
  }

  def manhattan: Int = east.abs + north.abs
}

case class WaypointLocation(ship: (Int, Int), waypoint: (Int, Int)) {
  private val regex = "([A-Z])([0-9]+)".r

  def move(moveDef: String): WaypointLocation = moveDef match {
    case regex(action, valueString) => {
      val value = valueString.toInt
      action match {
        case "N" => WaypointLocation(ship, (waypoint._1, waypoint._2 + value))
        case "S" => WaypointLocation(ship, (waypoint._1, waypoint._2 - value))
        case "E" => WaypointLocation(ship, (waypoint._1 + value, waypoint._2))
        case "W" => WaypointLocation(ship, (waypoint._1 - value, waypoint._2))
        case "L" => WaypointLocation(ship, rotateWaypoint(waypoint, action, value))
        case "R" => WaypointLocation(ship, rotateWaypoint(waypoint, action, value))
        case "F" => WaypointLocation((ship._1 + (waypoint._1 * value), ship._2 + (waypoint._2 * value)), waypoint)
      }
    }
  }

  def rotateWaypoint(waypoint: (Int, Int), direction: String, degrees: Int): (Int, Int) = {
    val turnUnits = (degrees / 90) % 4
    rotate(waypoint, rotations(direction), turnUnits)
  }

  @tailrec
  private def rotate(waypoint: (Int, Int), rotateFn: ((Int, Int)) => (Int, Int), times: Int): (Int, Int) =
    if (times == 0) waypoint
    else rotate(rotateFn(waypoint), rotateFn, times - 1)

  val rotations: Map[String, ((Int, Int)) => (Int, Int)] = Map("L" -> rotateLeft, "R" -> rotateRight)

  def rotateRight(waypoint: (Int, Int)): (Int, Int) = (waypoint._2, waypoint._1 * -1)

  def rotateLeft(waypoint: (Int, Int)): (Int, Int) = (waypoint._2 * -1, waypoint._1)

  def manhattan: Int = ship._1.abs + ship._2.abs
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
