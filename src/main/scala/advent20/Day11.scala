package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec

object Day11 extends App {
  val lines = new InputFileReader("input11").getLines.toIndexedSeq

  var lounge = Lounge(lines)

  while (!lounge.stable) {
    lounge = lounge.tick
  }
  println("Answer (part 1) " + lounge.occupied)

  var visibleLounge = Lounge(lines)
  while (!visibleLounge.stable) {
    visibleLounge = visibleLounge.tickVisible
  }
  println("Answer (part 2) " + visibleLounge.occupied)
}

case class Lounge(state: IndexedSeq[IndexedSeq[Tile]], prev: Option[Lounge]) {
  val occupied: Int = (for (row <- state;
                            tile <- row;
                            if (tile == Occupied)) yield 1).sum

  def tick: Lounge = tick(newTile)

  def tickVisible: Lounge = tick(newTileVisible)

  private def tick(newTile: (Tile, Int, Int) => Tile): Lounge = {
    val newState = for ((row, irow) <- state.zipWithIndex)
      yield {
        for ((tile, icol) <- row.zipWithIndex)
          yield newTile(tile, irow, icol)
      }
    new Lounge(newState, Some(this))
  }

  def stable: Boolean = prev match {
    case Some(previousLounge) => state == previousLounge.state
    case None => false
  }

  private def visibleNeighbours(irow: Int, icol: Int): Int = {
    @tailrec
    def firstVisible(irow: Int, icol: Int, direction: (Int, Int)): Option[Tile] = {
      val newrow = irow + direction._1
      val newcol = icol + direction._2
      if (indexIn(newrow, newcol))
        state(newrow)(newcol) match {
          case Floor => firstVisible(newrow, newcol, direction)
          case Empty => Some(Empty)
          case Occupied => Some(Occupied)
        }
      else None
    }

    val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

    val visibleNeighbours = directions map { direction =>
      firstVisible(irow, icol, direction) match {
        case Some(Occupied) => 1
        case Some(Empty) => 0
        case None => 0
      }
    }

    visibleNeighbours.sum
  }

  private def newTileVisible(tile: Tile, irow: Int, icol: Int): Tile = {
    val occupied = visibleNeighbours(irow, icol)

    tile match {
      case Floor => Floor
      case Empty => if (occupied == 0) Occupied else Empty
      case Occupied => if (occupied >= 5) Empty else Occupied
    }
  }

  private def newTile(tile: Tile, irow: Int, icol: Int): Tile = {
    val occupied = occupiedNeighbours(irow, icol)

    tile match {
      case Floor => Floor
      case Empty => if (occupied == 0) Occupied else Empty
      case Occupied => if (occupied >= 4) Empty else Occupied
    }
  }

  private def occupiedNeighbours(irow: Int, icol: Int): Int = {
    val neighbours = for (r <- List(irow - 1, irow, irow + 1);
                          c <- List(icol - 1, icol, icol + 1)
                          if !(r == irow && c == icol)
                          if indexIn(r, c)) yield state(r)(c)
    neighbours.count(_ == Occupied)
  }

  private def indexIn(irow: Int, icol: Int): Boolean =
    (irow >= 0 && irow < state.size) && (icol >= 0 && icol < state(irow).size)

  def printState(): Unit = {
    state.foreach(row => {
      row.foreach(print)
      println()
    })
    println()
  }
}

case object Lounge {
  def apply(l: IndexedSeq[String]): Lounge = new Lounge(l.map(line => line.map(Tile(_))), None)
}

sealed trait Tile

object Tile {
  def apply(c: Char): Tile = {
    c match {
      case '.' => Floor
      case 'L' => Empty
      case '#' => Occupied
    }
  }
}

case object Floor extends Tile {
  override def toString = "."
}

case object Empty extends Tile {
  override def toString = "L"
}

case object Occupied extends Tile {
  override def toString = "#"
}
