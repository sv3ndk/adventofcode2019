package advent


import scala.io.Source
import scala.util.Using
import Math._

import advent.Day10.{Coord, asteroids}

import scala.annotation.tailrec

object Day10Part1 extends App {

  val allAsteroids = {
    val allRawAsteroids = Using(Source.fromFile("src/main/data/day10-part1-map.txt")) { file => file.getLines().toList }.get
    asteroids(allRawAsteroids)
  }

  val best = Day10.bestMonitorLocation(allAsteroids)
  println(s"Advent of code 2019 - Day 10 / part 1: best location: $best") // (22, 28) => 326
}

object Day10Part2 extends App {
  val allAsteroids = {
    val allRawAsteroids = Using(Source.fromFile("src/main/data/day10-part1-map.txt")) { file => file.getLines().toList }.get
    asteroids(allRawAsteroids)
  }

  val ordered = Day10.orderlyKilledAsteroid(Coord(22, 28), allAsteroids)

  val twoHundreth = ordered(199)
  println(s"Advent of code 2019 - Day 10 / part 2: 200th: $twoHundreth, solution =${twoHundreth.x * 100 + twoHundreth.y}")
}


object Day10 extends App {

  case class Coord(x: Int, y: Int) {
    def -(other: Coord): Slope = Slope(x - other.x, y - other.y)

    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)

    // angle between the vertical line and that asteroid
    def angleFromVer(reference: Coord): Double = {
      val shifted = this - reference

      if (shifted.dx == 0 && shifted.dy <= 0) 0d // y axis is flipped compared to usual

      else if (shifted.dx == 0 && shifted.dy > 0) math.Pi // y axis is flipped compared to usual

      else {
        if (shifted.dx >= 0) -(math.atan(-shifted.dy.toDouble / shifted.dx.toDouble) - math.Pi / 2)
        else math.Pi - (math.atan(-shifted.dy.toDouble / shifted.dx.toDouble) - math.Pi / 2)
      } // y axis is flipped compared to usual

    }

    override def toString: String = "[" + x + ", " + y + ", " + angleFromVer(Coord(8, 3)) + "] "
  }

  def distance(p1: Coord, p2: Coord): Double = sqrt(pow(p1.x.toDouble - p2.x.toDouble, 2d) + pow(p1.y.toDouble - p2.y.toDouble, 2d))

  case class Slope(dx: Int, dy: Int) {
    lazy val simplified: Slope = {
      val g = Math.abs(gcd(dx, dy))
      if (g == 0) this
      else Slope(dx / g, dy / g)
    }
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // asteroid from which most other asteroid are visible
  def bestMonitorLocation(asteroidCoords: List[Coord]): (Coord, Int) =
    asteroidCoords
      .map(a => (a, visibility(a, asteroidCoords)))
      .maxBy(_._2)

  // list of all asteroid coordinates
  def asteroids(map: List[String]): List[Coord] =
    map
      .zipWithIndex
      .flatMap { case (line, y) =>
        line
          .zipWithIndex
          .filter { case (symbol, x) => symbol == '#' || symbol == 'X' }
          .map { case (symbol, x) => Coord(x, y) }
      }

  // number of asteroids directly in sight from that reference
  def visibility(reference: Coord, allAsteroids: List[Coord]): Int =
    allAsteroids
      .filter(_ != reference)
      .map { c => (c - reference).simplified }
      .distinct
      .size

  // sequence of "layers" of asteroids that are destroyed one after the other
  def layers(reference: Coord, allAsteroids: List[Coord]): List[Set[Coord]] = {

    def nextLayer(remaingAsteroids: Set[Coord]) =
      remaingAsteroids

        // grouping by slope from the center
        .map { c => (c, (c - reference).simplified) }
        .groupBy(_._2)
        .values
        .map(set => set.map(_._1))

        // for each slope, keep the closest one
        .map(coords => coords.minBy(coord => distance(coord, reference)))
        .toSet

    @tailrec
    def allLayers(remaingAsteroids: Set[Coord], prevLayers: List[Set[Coord]]): List[Set[Coord]] = {
      val oneMoreLayer = nextLayer(remaingAsteroids)
      if (oneMoreLayer.isEmpty) prevLayers.reverse
      else allLayers(remaingAsteroids.diff(oneMoreLayer), oneMoreLayer :: prevLayers)
    }

    allLayers(allAsteroids.toSet.filter(_ != reference), List.empty)
  }

  // part 2: ordered list of killed asteroids
  def orderlyKilledAsteroid(reference: Coord, allAsteroids: List[Coord]): List[Coord] =
    layers(reference, allAsteroids)
      .flatMap {
        layer =>
          layer.toList
            .sortBy(asteroid => asteroid.angleFromVer(reference))
      }
}
