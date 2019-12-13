package advent

import advent.Day9.IntCodeComputer.State
import advent.Robot.Coord

import scala.annotation.tailrec

object Day11Part1 extends App {
  // for once it seems the existing IntCode program will do
  val day11Program = Day9.loadProgram("src/main/data/day11-painting-program.txt")
  val paintedShip = Robot.paintItBlack(day11Program, Map.empty)
  println(s"Advent of code 2019 - Day 11 / part 1: number of painted squares: ${paintedShip.size}") // 1932
}

object Day11Part2 extends App {
  // for once it seems the existing IntCode program will do
  val day11Program = Day9.loadProgram("src/main/data/day11-painting-program.txt")
  val paintedShip = Robot.paintItBlack(day11Program, Map(Coord(0, 0) -> Robot.WHITE))

  println(s"Advent of code 2019 - Day 11 / part 2") // EGHKGJER

  val min_x = paintedShip.map(_._1.x).min
  val max_x = paintedShip.map(_._1.x).max
  val min_y = paintedShip.map(_._1.y).min
  val max_y = paintedShip.map(_._1.y).max

  // flipping axis in order to fit something
  (min_y to max_y).reverse.foreach {
    y =>
      (min_x to max_x).foreach {
        x =>
          paintedShip.getOrElse(Coord(x, y), Robot.BLACK) match {
            case Robot.BLACK => print(" ")
            case Robot.WHITE => print("|")
          }
      }
      print("\n")
  }
}

object Robot {

  val BLACK = 0
  val WHITE = 1

  object Orientation extends Enumeration {
    type Orientation = Value
    val UP, DOWN, LEFT, RIGHT = Value
  }

  // x,y coordinate. positive x is to the right, positive y is up
  case class Coord(x: Int, y: Int) {
    def up(distance: Int) = Coord(x, y + distance)

    def down(distance: Int) = Coord(x, y - distance)

    def right(distance: Int) = Coord(x + distance, y)

    def left(distance: Int) = Coord(x - distance, y)
  }

  case class Ship(position: Coord, orientation: Orientation.Orientation) {
    lazy val turnLeft: Ship = copy(orientation = orientation match {
      case Orientation.UP => Orientation.LEFT
      case Orientation.LEFT => Orientation.DOWN
      case Orientation.DOWN => Orientation.RIGHT
      case Orientation.RIGHT => Orientation.UP
    })

    lazy val turnRight: Ship = copy(orientation = orientation match {
      case Orientation.UP => Orientation.RIGHT
      case Orientation.LEFT => Orientation.UP
      case Orientation.DOWN => Orientation.LEFT
      case Orientation.RIGHT => Orientation.DOWN
    })

    def forward(distance: Int): Ship = orientation match {
      case Orientation.UP => copy(position = position.up(distance))
      case Orientation.LEFT => copy(position = position.left(distance))
      case Orientation.DOWN => copy(position = position.down(distance))
      case Orientation.RIGHT => copy(position = position.right(distance))
    }
  }

  def paintItBlack(program: Seq[Long], initPainting: Map[Coord, Int]): Map[Coord, Int] = {

    @tailrec
    def loop(shipPaint: Map[Coord, Int], ship: Ship, prevState: Option[State]): Map[Coord, Int] = {

      val currentColor = shipPaint.getOrElse(ship.position, BLACK)

      val state = prevState match {
        case Some(state) => Day9.IntCodeComputer.resume(state, currentColor)
        case None => Day9.IntCodeComputer.start(program, currentColor)
      }

      val Seq(paintedColor, whereToNow) = state.output

      val newPaint = shipPaint + (ship.position -> paintedColor.intValue)

      val newShip = whereToNow match {
        case 0 => ship.turnLeft.forward(1)
        case 1 => ship.turnRight.forward(1)
      }

      if (state.isPaused) loop(newPaint, newShip, Some(state))
      else shipPaint // it also never painted the panel it ended on -> keeping the paint as it was before the last move)
    }

    loop(initPainting, Ship(Coord(0, 0), Orientation.UP), None)
  }

}
