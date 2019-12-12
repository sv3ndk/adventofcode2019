package advent

import advent.Day10._
import org.scalatest.{FlatSpec, Matchers}

class Day10Spec extends FlatSpec with Matchers{

  "visibility from example" should "match" in {

    val p =
      """.#..#
        |.....
        |#####
        |....#
        |...##
        |""".stripMargin.split("\n").toList

    val asteroidMap = asteroids(p)

    //.7..7
    //.....
    //67775
    //....7
    //...87

    visibility(Coord(1, 0), asteroidMap) should be (7)
    visibility(Coord(4, 0), asteroidMap) should be (7)
    visibility(Coord(0, 2), asteroidMap) should be (6)
    visibility(Coord(1, 2), asteroidMap) should be (7)
    visibility(Coord(2, 2), asteroidMap) should be (7)
    visibility(Coord(3, 2), asteroidMap) should be (7)
    visibility(Coord(4, 2), asteroidMap) should be (5)
    visibility(Coord(4, 3), asteroidMap) should be (7)
    visibility(Coord(3, 4), asteroidMap) should be (8)
    visibility(Coord(4, 4), asteroidMap) should be (7)

    bestMonitorLocation(asteroidMap) should be ((Coord(3,4), 8))

  }

  "example 2" should "match "in {

    val p ="""......#.#.
             |#..#.#....
             |..#######.
             |.#.#.###..
             |.#..#.....
             |..#....#.#
             |#..#....#.
             |.##.#..###
             |##...#..#.
             |.#....####""".stripMargin.split("\n").toList

    val asteroidMap = asteroids(p)

    bestMonitorLocation(asteroidMap) should be ((Coord(5,8), 33))

  }

  "example 11" should "match "in {

    val p =""".#..##.###...#######
             |##.############..##.
             |.#.######.########.#
             |.###.#######.####.#.
             |#####.##.#.##.###.##
             |..#####..#.#########
             |####################
             |#.####....###.#.#.##
             |##.#################
             |#####.##.###..####..
             |..######..##.#######
             |####.##.####...##..#
             |.#####..#.######.###
             |##...#.##########...
             |#.##########.#######
             |.####.#.###.###.#.##
             |....##.##.###..#####
             |.#.#.###########.###
             |#.#.#.#####.####.###
             |###.##.####.##.#..##""".stripMargin.split("\n").toList

    val asteroidMap = asteroids(p)

    bestMonitorLocation(asteroidMap) should be ((Coord(11,13), 210))
  }

  "layers from simple example" should "be a list of 2" in {

    val p =
      """.#..#
        |.....
        |#####
        |....#
        |...##
        |""".stripMargin.split("\n").toList

    Day10.layers(Coord(3,4), asteroids(p)) should be (List(
      // first layer; we see pretty much everything, except 1 asteroid
      Set(Coord(4,4), Coord(2,2), Coord(4,2), Coord(4,0), Coord(0,2), Coord(3,2), Coord(1,2), Coord(4,3)),

      // last one
      Set(Coord(1,0))
    ))
  }

  "ordered kills" should "be as visually expected" in {

    val p =
      """.#..#
        |.....
        |#####
        |....#
        |...##
        |""".stripMargin.split("\n").toList

    Day10.orderlyKilledAsteroid(Coord(3, 4), asteroids(p)) should be (List(Coord(0,-2), Coord(1,-4), Coord(1,-2), Coord(1,-1), Coord(1,0), Coord(-3,-2), Coord(-2,-2), Coord(-1,-2), Coord(-2,-4)))
  }

  "other exzmple" should "match as well " in {

    val p =""".#....#####...#..
             |##...##.#####..##
             |##...#...#.#####.
             |..#.....X...###..
             |..#.#.....#....##""".stripMargin.split("\n").toList

    Day10.orderlyKilledAsteroid(Coord(8,3), asteroids(p)).take(27) should be (List(

      // .#....###24...#..
      // ##...##.13#67..9#
      // ##...#...5.8####.
      // ..#.....X...###..
      // ..#.#.....#....##
      Coord(8,1),
      Coord(9,0),
      Coord(9,1),
      Coord(10,0),
      Coord(9,2), //5
      Coord(11,1), // 6
      Coord(12,1),
      Coord(11,2), // 8
      Coord(15,1), // 9


      // 01234567890123456
      // .#....###.....#..
      // ##...##...#.....#
      // ##...#......1234.
      // ..#.....X...5##..
      // ..#.9.....8....76
      Coord(12,2),
      Coord(13,2),
      Coord(14,2),
      Coord(15,2),
      Coord(12,3), //15
      Coord(16,4),
      Coord(15,4),
      Coord(10,4),
      Coord(4,4),


      // 01234567890123456
      // .8....###.....#..
      // 56...9#...#.....#
      // 34...7...........
      // ..2.....X....##..
      // ..1..............
      Coord(2,4), //1
      Coord(2,3),
      Coord(0,2),
      Coord(1,2),
      Coord(0,1), //5
      Coord(1,1),
      Coord(5,2),
      Coord(1,0), // 8
      Coord(5,1),


    ))

  }


  "example ordered kills" should "be as expected " in {
    val p =
      """.#..##.###...#######
        |##.############..##.
        |.#.######.########.#
        |.###.#######.####.#.
        |#####.##.#.##.###.##
        |..#####..#.#########
        |####################
        |#.####....###.#.#.##
        |##.#################
        |#####.##.###..####..
        |..######..##.#######
        |####.##.####...##..#
        |.#####..#.######.###
        |##...#.####X#####...
        |#.##########.#######
        |.####.#.###.###.#.##
        |....##.##.###..#####
        |.#.#.###########.###
        |#.#.#.#####.####.###
        |###.##.####.##.#..##""".stripMargin.split("\n").toList

    val ordered = Day10.orderlyKilledAsteroid(Coord(11,13), asteroids(p))

      ordered(0) should be (Coord(11,12))
      ordered(1) should be (Coord(12,1))
      ordered(2) should be (Coord(12,2))

      ordered(9) should be (Coord(12,8))
      ordered(19) should be (Coord(16,0))
      ordered(49) should be (Coord(16,9))
      ordered(99) should be (Coord(10,16))
      ordered(198) should be (Coord(9,6))
      ordered(199) should be (Coord(8,2))
      ordered(200) should be (Coord(10,9))

  }

}
