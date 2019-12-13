package advent

import advent.Day12.Moon
import advent.Day12.Vect
import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {

  "moving moons around" should "let them arrive at the correct location" in {

    //"Io", "Europa", "Ganymede", "Callisto"
    val rawMoons ="""<x=-1, y=0, z=2>
                    |<x=2, y=-10, z=-7>
                    |<x=4, y=-8, z=8>
                    |<x=3, y=5, z=-1>"""
      .stripMargin.split("\n").toList

    val moons = Day12.parseMoons(rawMoons)

    // After 1 step:
    // pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
    // pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
    // pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
    // pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>

    val step1 = Day12.updateAllMoonsOnce(moons)
    val step1Map = Day12.moonmap(step1)

    step1Map("Io") should be (Moon("Io", Vect(2, -1, 1), Vect(3, -1, -1)))
    step1Map("Europa") should be (Moon("Europa", Vect(3, -7, -4), Vect(1, 3, 3)))
    step1Map("Ganymede") should be (Moon("Ganymede", Vect(1, -7, 5), Vect(-3, 1, -3)))
    step1Map("Callisto") should be (Moon("Callisto", Vect(2, 2, 0), Vect(-1, -3, 1)))

    // After 2 steps:
    // pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
    // pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
    // pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
    // pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>

    val step2 = Day12.updateAllMoonsOnce(step1)
    val step2Map = Day12.moonmap(step2)

    step2Map("Io") should be (Moon("Io", Vect(5, -3, -1), Vect(3, -2, -2)))
    step2Map("Europa") should be (Moon("Europa", Vect(1, -2, 2), Vect(-2, 5, 6)))
    step2Map("Ganymede") should be (Moon("Ganymede", Vect(1, -4, -1), Vect(0, 3, -6)))
    step2Map("Callisto") should be (Moon("Callisto", Vect(1, -4, 2), Vect(-1, -6, 2)))

    println(Day12.runNSteps(10, moons))

    Day12.runNSteps(10, moons).map(_.energy).sum should be (179)

  }

  "brute force part 2" should "repeat itelf" in {

    //"Io", "Europa", "Ganymede", "Callisto"
    val rawMoons =
      """<x=-1, y=0, z=2>
        |<x=2, y=-10, z=-7>
        |<x=4, y=-8, z=8>
        |<x=3, y=5, z=-1>"""
        .stripMargin.split("\n").toList

    val moons = Day12.parseMoons(rawMoons)

    Day12.bruteForceStep2bis(moons) should be (2772)
  }

  "LCM part 2" should "repeat itelf" in {

    //"Io", "Europa", "Ganymede", "Callisto"
    val rawMoons =
      """<x=-1, y=0, z=2>
        |<x=2, y=-10, z=-7>
        |<x=4, y=-8, z=8>
        |<x=3, y=5, z=-1>"""
        .stripMargin.split("\n").toList

    Day12Part2.systemPeriod(rawMoons) should be (2772)
  }

  "other part 2 example" should "repeat itelf" in {

    //"Io", "Europa", "Ganymede", "Callisto"
    val rawMoons =
      """<x=-8, y=-10, z=0>
        |<x=5, y=5, z=10>
        |<x=2, y=-7, z=3>
        |<x=9, y=-8, z=-3>"""
        .stripMargin.split("\n").toList

    Day12Part2.systemPeriod(rawMoons) should be (4686774924L)
  }

  }

