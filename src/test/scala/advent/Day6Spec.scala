package advent

import org.scalatest.{FlatSpec, Matchers}

class Day6Spec extends FlatSpec with Matchers {

  "example tree" should "be parsed correctly"in {
    val raw = Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    Day6.orbitCount(raw) should be (42)
  }

  "another example tree" should "be ok too " in {
    val raw = Seq("G)H", "B)C", "C)D", "D)E", "E)F", "COM)B", "B)G", "D)I", "E)J", "J)K", "K)L", "I)M")
    Day6.orbitCount(raw) should be (47)
  }

}
