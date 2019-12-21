package advent

import org.scalatest.{FlatSpec, Matchers}
import Day14._

class Day14Test extends FlatSpec with Matchers {


  val cookbook1 = Map(
    "A" -> Recipe(10, Set(Chemical("ORE", 10))),
    "B" -> Recipe(1, Set(Chemical("ORE", 1))),
    "C" -> Recipe(1, Set(Chemical("A", 7), Chemical("B", 1))),
    "D" -> Recipe(1, Set(Chemical("A", 7), Chemical("C", 1))),
    "E" -> Recipe(1, Set(Chemical("A", 7), Chemical("D", 1))),
    "FUEL" -> Recipe(1, Set(Chemical("A", 7), Chemical("E", 1)))
  )

  val cookbook2 = cookBook(
    """9 ORE => 2 A
      |8 ORE => 3 B
      |7 ORE => 5 C
      |3 A, 4 B => 1 AB
      |5 B, 7 C => 1 BC
      |4 C, 1 A => 1 CA
      |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin.linesIterator.toList)

  val cookbook3 = cookBook(
    """157 ORE => 5 NZVS
      |165 ORE => 6 DCFZ
      |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
      |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
      |179 ORE => 7 PSHF
      |177 ORE => 5 HKGWZ
      |7 DCFZ, 7 PSHF => 2 XJWVT
      |165 ORE => 2 GPVTF
      |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin.linesIterator.toList)

  val cookbook4 = cookBook(
    """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
      |17 NVRVD, 3 JNWZP => 8 VPVL
      |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
      |22 VJHF, 37 MNCFX => 5 FWMGM
      |139 ORE => 4 NVRVD
      |144 ORE => 7 JNWZP
      |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
      |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
      |145 ORE => 6 MNCFX
      |1 NVRVD => 8 CXFTF
      |1 VJHF, 6 MNCFX => 4 RFSQX
      |176 ORE => 6 VJHF""".stripMargin.linesIterator.toList)

  val cookbook5 = cookBook(
    """171 ORE => 8 CNZTR
      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
      |114 ORE => 4 BHXH
      |14 VRPVC => 6 BMBT
      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
      |5 BMBT => 4 WPTQ
      |189 ORE => 9 KTJDG
      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
      |12 VRPVC, 27 CNZTR => 2 XDBXC
      |15 KTJDG, 12 BHXH => 5 XCVML
      |3 BHXH, 2 VRPVC => 7 MZWV
      |121 ORE => 7 VRPVC
      |7 XCVML => 6 RJRHP
      |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin.linesIterator.toList)

  "basic example" should "provide expected result" in {
    val puzzle =
      """10 ORE => 10 A
        |1 ORE => 1 B
        |7 A, 1 B => 1 C
        |7 A, 1 C => 1 D
        |7 A, 1 D => 1 E
        |7 A, 1 E => 1 FUEL""".stripMargin.linesIterator.toList

    cookBook(puzzle) should be(cookbook1)
  }

  "simple example " should " require 31 ores" in {
    Day14Part1.requiredOresForOneFuel(cookbook1) should be(31)
  }

  "second recipe" should "require 165 ORE" in {
    Day14Part1.requiredOresForOneFuel(cookbook2) should be(165)
  }

  "third recipe" should "require 13312 ORE" in {
    Day14Part1.requiredOresForOneFuel(cookbook3) should be(13312)
  }

  "4th recipe" should "require 180697 ORE" in {
    Day14Part1.requiredOresForOneFuel(cookbook4) should be(180697)
  }

  "5th recipe" should "require 2210736 ORE" in {
    Day14Part1.requiredOresForOneFuel(cookbook5) should be(2210736)
  }

  ////
  /// part 2

  //
  //  "brute forcing 5th recipe" should "produce 1T fuel" in {
  //
  //    // this works, though takes 6 minutes...
  //
  //    val puzzle = """171 ORE => 8 CNZTR
  //                   |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
  //                   |114 ORE => 4 BHXH
  //                   |14 VRPVC => 6 BMBT
  //                   |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
  //                   |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
  //                   |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
  //                   |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
  //                   |5 BMBT => 4 WPTQ
  //                   |189 ORE => 9 KTJDG
  //                   |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
  //                   |12 VRPVC, 27 CNZTR => 2 XDBXC
  //                   |15 KTJDG, 12 BHXH => 5 XCVML
  //                   |3 BHXH, 2 VRPVC => 7 MZWV
  //                   |121 ORE => 7 VRPVC
  //                   |7 XCVML => 6 RJRHP
  //                   |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin.linesIterator.toList
  //
  //    Day14Part2.fuelFor1TOresBruteForce(cookBook(puzzle)) should be (460664)
  //  }


  def extrapolationTest(cookBook: Map[String, Recipe]) = {
    val (ores1, leftOvers1) = Day14.totalOres(0, Map("FUEL" -> 1), Map.empty)(cookBook)
    val (ores2, leftOvers2) = Day14.totalOres(ores1, Map("FUEL" -> 1), leftOvers1)(cookBook)
    val (ores3, leftOvers3) = Day14.totalOres(ores2, Map("FUEL" -> 1), leftOvers2)(cookBook)
    val (ores4, leftOvers4) = Day14.totalOres(ores3, Map("FUEL" -> 1), leftOvers3)(cookBook)
    val (ores5, leftOvers5) = Day14.totalOres(ores4, Map("FUEL" -> 1), leftOvers4)(cookBook)
    val (ores6, leftOvers6) = Day14.totalOres(ores5, Map("FUEL" -> 1), leftOvers5)(cookBook)
    val (ores7, leftOvers7) = Day14.totalOres(ores6, Map("FUEL" -> 1), leftOvers6)(cookBook)
    val (ores8, leftOvers8) = Day14.totalOres(ores7, Map("FUEL" -> 1), leftOvers7)(cookBook)
    val (ores9, leftOvers9) = Day14.totalOres(ores8, Map("FUEL" -> 1), leftOvers8)(cookBook)
    val (ores10, leftOvers10) = Day14.totalOres(ores9, Map("FUEL" -> 1), leftOvers9)(cookBook)

    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 2)(cookBook) should be(2, ores2, leftOvers2)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 3)(cookBook) should be(3, ores3, leftOvers3)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 4)(cookBook) should be(4, ores4, leftOvers4)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 5)(cookBook) should be(5, ores5, leftOvers5)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 6)(cookBook) should be(6, ores6, leftOvers6)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 7)(cookBook) should be(7, ores7, leftOvers7)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 8)(cookBook) should be(8, ores8, leftOvers8)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 9)(cookBook) should be(9, ores9, leftOvers9)
    Day14Part2.extrapolatedCost(1, ores1, leftOvers1, 10)(cookBook) should be(10, ores10, leftOvers10)

    Day14Part2.extrapolatedCost(2, ores2, leftOvers2, 2)(cookBook) should be(4, ores4, leftOvers4)
    Day14Part2.extrapolatedCost(2, ores2, leftOvers2, 3)(cookBook) should be(6, ores6, leftOvers6)
    Day14Part2.extrapolatedCost(2, ores2, leftOvers2, 4)(cookBook) should be(8, ores8, leftOvers8)

    Day14Part2.extrapolatedCost(3, ores3, leftOvers3, 2)(cookBook) should be(6, ores6, leftOvers6)
    Day14Part2.extrapolatedCost(3, ores3, leftOvers3, 3)(cookBook) should be(9, ores9, leftOvers9)

    Day14Part2.extrapolatedCost(4, ores4, leftOvers4, 2)(cookBook) should be(8, ores8, leftOvers8)

    Day14Part2.extrapolatedCost(5, ores5, leftOvers5, 2)(cookBook) should be(10, ores10, leftOvers10)

  }

  "extrapolated costs for puzzle 1" should "match simulation" in {
    extrapolationTest(cookbook1)
  }
  "extrapolated costs for puzzle 2" should "match simulation" in {
    extrapolationTest(cookbook2)
  }
  "extrapolated costs for puzzle 3" should "match simulation" in {
    extrapolationTest(cookbook3)
  }
  "extrapolated costs for puzzle 4" should "match simulation" in {
    extrapolationTest(cookbook4)
  }
  "extrapolated costs for puzzle 5" should "match simulation" in {
    extrapolationTest(cookbook5)
  }

  "fuel for 1T ores in puzzle3" should "match" in {
    Day14Part2.fuelFor1TOres(cookbook3) should be (82892753)
  }

  "fuel for 1T ores in puzzle4" should "match" in {
    Day14Part2.fuelFor1TOres(cookbook4) should be (5586022)
  }

  "fuel for 1T ores in puzzle5" should "match" in {
    Day14Part2.fuelFor1TOres(cookbook5) should be (460664)
  }

}
