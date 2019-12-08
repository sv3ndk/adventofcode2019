package advent


import scala.io.Source
import scala.util.Using

object Day3Part1 extends App {

  Using(Source.fromFile("src/main/data/day3-move-sequence.txt")) {
    file => {
      val rawMoves1 :: rawMoves2 :: Nil = file.getLines().toSeq
      val result = Day3.closestIntersectionDistance(rawMoves1, rawMoves2)
      println(s"Advent of code 2019 - Day 3 / part 1: closest dist to intersection: ${result}")
    }
  }

}

object Day3 {

  val centralPort = Location(0, 0)

  case class Location(x: Int, y: Int)

  trait Move {
    // seq of visited locations when applying that move from that position
    def from(location: Location): Seq[Location]
  }

  case class GoLeft(distance: Int) extends Move {
    override def from(start: Location): Seq[Location] = (1 to distance).map(d => Location(start.x - d, start.y))
  }

  case class GoRight(distance: Int) extends Move {
    override def from(start: Location): Seq[Location] = (1 to distance).map(d => Location(start.x + d, start.y))
  }

  case class GoUp(distance: Int) extends Move {
    override def from(start: Location): Seq[Location] = (1 to distance).map(d => Location(start.x, start.y + d))
  }

  case class GoDown(distance: Int) extends Move {
    override def from(start: Location): Seq[Location] = (1 to distance).map(d => Location(start.x, start.y - d))
  }

  def asMove(move: String): Move = {

    val direction = move.take(1)
    val distance = move.drop(1).toInt

    direction match {
      case "U" => GoUp(distance)
      case "D" => GoDown(distance)
      case "L" => GoLeft(distance)
      case "R" => GoRight(distance)
      case _ => throw new RuntimeException(s"unrecognized move: ${move}")
    }
  }

  // parses a move sequence like "R1001,D915,R511,D336,..." into the corresponding Moves
  def parsedMoves(rawMoves: String): Seq[Move] = rawMoves.split(",").map(asMove)

  // seq of visited location when applying those moves from that location
  def visitedLocations(fromLocation: Location)(moves: Seq[Move]): Seq[Location] =
    moves.foldLeft(Seq(fromLocation)) { (locs, move) => locs ++ move.from(locs.last) }

  // intersection locations of those 2 paths
  def intersections(path1: Seq[Location], path2: Seq[Location]): Set[Location] =
    path1.toSet.intersect(path2.toSet).diff(Set())

  def manathanDistance(location: Location): Int = location.x.abs + location.y.abs

  // finds the distance from the central port to the closest intersection
  def closestIntersectionDistance(rawMoves1: String, rawMoves2: String): Int =
    Day3
      .intersections(
        Day3.visitedLocations(centralPort)(Day3.parsedMoves(rawMoves1)),
        Day3.visitedLocations(centralPort)(Day3.parsedMoves(rawMoves2)))
      .diff(Set(centralPort))
      .map(Day3.manathanDistance)
      .reduce(Math.min)

}
