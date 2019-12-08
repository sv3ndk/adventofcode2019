package advent

import advent.Day3._
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "parsing examples" should "work as expected" in {
    Day3.asMove("R75") should be (GoRight(75))
    Day3.asMove("D30") should be (GoDown(30))
    Day3.asMove("U83") should be (GoUp(83))
    Day3.asMove("L12") should be (GoLeft(12))
    Day3.asMove("D49") should be (GoDown(49))
    Day3.asMove("R71") should be (GoRight(71))
    Day3.asMove("U7") should be (GoUp(7))
    Day3.asMove("L72") should be (GoLeft(72))
  }

  "parsing a sequence of moves" should "provide the expected sequence" in {
    Day3.parsedMoves("R1001,D915,R511,D336,L647,D844,R97,D579,L336,U536") should be (
      Seq(
        GoRight(1001), GoDown(915), GoRight(511), GoDown(336), GoLeft(647),
        GoDown(844), GoRight(97), GoDown(579), GoLeft(336), GoUp(536))
    )
  }

  "applying moves from 0,0" should "end up in the correct location" in {
    GoLeft(4) from(Location(0, 0)) should be (Seq(Location(-1, 0), Location(-2, 0), Location(-3, 0), Location(-4, 0)))
    GoRight(4) from(Location(0, 0)) should be (Seq(Location(1, 0), Location(2, 0), Location(3, 0), Location(4, 0)))
    GoUp(4) from(Location(0, 0)) should be (Seq(Location(0, 1), Location(0, 2), Location(0, 3), Location(0, 4)))
    GoDown(4) from(Location(0, 0)) should be (Seq(Location(0, -1), Location(0, -2), Location(0, -3), Location(0, -4)))
  }

  "applying moves from 0,0" should "provide the expected locations" in {

    println(visitedLocations(Location(0, 0) )(Seq(GoUp(4), GoRight(2), GoDown(1), GoLeft(5))))

    visitedLocations(Location(0, 0))(Seq(GoUp(4), GoRight(2), GoDown(1), GoLeft(5))) should be (Seq(
      // starting location
      Location(0, 0),

      // up 4
      Location(0, 1), Location(0, 2), Location(0, 3), Location(0, 4),

      // right 2
      Location(1, 4), Location(2, 4),

      // down 1
      Location(2, 3),

      // left 5
      Location(1, 3), Location(0, 3), Location(-1, 3), Location(-2, 3), Location(-3, 3)))
  }

  "example closest intersection distance " should "match" in {

    Day3.closestIntersectionDistance(
      "R75,D30,R83,U83,L12,D49,R71,U7,L72",
      "U62,R66,U55,R34,D71,R55,D58,R83"
    ) should be (159)

    Day3.closestIntersectionDistance(
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    ) should be (135)

  }

}
