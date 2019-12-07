package advent

import org.scalatest._

class Day1Spec extends FlatSpec with Matchers {

  "example fuel value for part 1" should "match" in {
    Day1.fuelFor(12) should be(2)
    Day1.fuelFor(14) should be(2)
      Day1.fuelFor(1969) should be(654)
      Day1.fuelFor(100756) should be(33583)
    }

  "example fuel value for part 2" should "match" in {
    Day1.totalFuelFor(14) should be(2)
    Day1.totalFuelFor(1969) should be(966)
    Day1.totalFuelFor(100756) should be(50346)
  }

}
