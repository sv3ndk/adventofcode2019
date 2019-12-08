package advent

import scala.io.Source
import scala.util.Using

object Day3Parts extends App {

  Using(Source.fromFile("src/main/data/day3-move-sequence.txt")) {
    file => {
      val rawMoves1 :: rawMoves2 :: Nil = file.getLines().toSeq

      val resultPart1 = closestIntersectionDistance(rawMoves1, rawMoves2)
      println(s"Advent of code 2019 - Day 3 / part 1: closest dist to intersection: ${resultPart1}")   // 248

      val resultPart2 = shortestIntersectionTiming(rawMoves1, rawMoves2)
      println(s"Advent of code 2019 - Day 3 / part 2: shortest timing to intersection: ${resultPart2}")   // 248
    }
  }
  // finds the distance from the central port to the closest intersection
  def closestIntersectionDistance(rawMoves1: String, rawMoves2: String): Int =
    Day3
      .intersections(
        Day3.path(Day3.centralPort)(Day3.parsedMoves(rawMoves1)),
        Day3.path(Day3.centralPort)(Day3.parsedMoves(rawMoves2)))
      .diff(Set(Day3.centralPort))
      .map(Day3.manathanDistance)
      .reduce(Math.min)

  // finds the distance from the central port to the closest intersection
  def shortestIntersectionTiming(rawMoves1: String, rawMoves2: String): Int = {

    val path1 = Day3.path(Day3.centralPort)(Day3.parsedMoves(rawMoves1))
    val path2 = Day3.path(Day3.centralPort)(Day3.parsedMoves(rawMoves2))

    Day3
      .intersections(path1, path2)
      .diff(Set(Day3.centralPort))
      .map(intersection => Day3.timingAlongPath(path1, intersection) + Day3.timingAlongPath(path2, intersection))
      .reduce(Math.min)
  }
}

object Day3 {

  val centralPort = Location(0, 0)

  case class Location(x: Int, y: Int)
  type Path = Seq[Location]

  trait Move {
    // seq of visited locations when applying that move from that position
    def from(location: Location): Path
  }

  case class GoLeft(distance: Int) extends Move {
    override def from(start: Location): Path = (1 to distance).map(d => Location(start.x - d, start.y))
  }

  case class GoRight(distance: Int) extends Move {
    override def from(start: Location): Path = (1 to distance).map(d => Location(start.x + d, start.y))
  }

  case class GoUp(distance: Int) extends Move {
    override def from(start: Location): Path = (1 to distance).map(d => Location(start.x, start.y + d))
  }

  case class GoDown(distance: Int) extends Move {
    override def from(start: Location): Path = (1 to distance).map(d => Location(start.x, start.y - d))
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
  def path(fromLocation: Location)(moves: Seq[Move]): Path =
    moves.foldLeft(Seq(fromLocation)) { (locs, move) => locs ++ move.from(locs.last) }

  // intersection locations of those 2 paths
  def intersections(path1: Path, path2: Path): Set[Location] = path1.toSet.intersect(path2.toSet).diff(Set())

  def manathanDistance(location: Location): Int = location.x.abs + location.y.abs

  def timingAlongPath(path: Path, location: Location): Int = path.indexOf(location)

}
