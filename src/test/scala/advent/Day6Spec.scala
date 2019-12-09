package advent

import org.scalatest.{FlatSpec, Matchers}

class Day6Spec extends FlatSpec with Matchers {

  //         G - H       J - K - L
  //       /           /
  //COM - B - C - D - E - F
  //               \
  //                I

  "example tree" should "be parsed correctly"in {
    val raw = Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    Day6Parts.solvePart1(raw) should be (42)
  }

  "another example tree" should "be ok too " in {
    val raw = Seq("G)H", "B)C", "C)D", "D)E", "E)F", "COM)B", "B)G", "D)I", "E)J", "J)K", "K)L", "I)M")
    Day6Parts.solvePart1(raw) should be (47)
  }

  "finding path to root" should "be be correct"in {
    val raw = Seq("COM" + ")B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    val childToParentTree = Day6.childToParentTree(raw)
    Day6.pathFromRoot(childToParentTree, "B") should be ("COM" +: "B" +: Nil)
    Day6.pathFromRoot(childToParentTree, "D") should be ("COM" +: "B" +: "C" +: "D" +: Nil)
    Day6.pathFromRoot(childToParentTree, "L") should be ("COM" +: "B" +: "C" +: "D" +: "E" +: "J" +: "K" +: "L" +: Nil)
  }

  "finding the deepest common ancestor" should " be correct" in {
    val raw = Seq("COM" + ")B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    val childToParentTree = Day6.childToParentTree(raw)

    Day6.deepestCommonAncestor(childToParentTree, "D", "H") should be ("B")
    Day6.deepestCommonAncestor(childToParentTree, "L", "H") should be ("B")
    Day6.deepestCommonAncestor(childToParentTree, "J", "I") should be ("D")
    Day6.deepestCommonAncestor(childToParentTree, "K", "F") should be ("E")
    Day6.deepestCommonAncestor(childToParentTree, "C", "G") should be ("B")
  }

  //                           YOU
  //                         /
  //        G - H       J - K - L
  //       /           /
  //COM - B - C - D - E - F
  //               \
  //                I - SAN

  "orbital transfers" should "be correct" in {

    val raw = Seq("COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L",
    "K)YOU",
    "I)SAN")

    val childToParentTree = Day6.childToParentTree(raw)

    Day6Parts.solvePart2(raw) should be (4)
  }

}

