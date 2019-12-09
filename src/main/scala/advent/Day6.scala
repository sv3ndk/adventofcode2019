package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day6Part1 extends App {
  Using(Source.fromFile("src/main/data/day6-orbits.txt")) {
    file =>
      val sumOrbitCount = Day6.orbitCount(file.getLines().toSeq)
      println(s"Advent of code 2019 - Day 6 / part 1: orbit count: $sumOrbitCount")
  }
}

object Day6 {

  def parseOrbit(rawOrbit: String) = {
    val Seq(parentId, childId) = rawOrbit.split(("\\)")).toSeq
    (parentId.trim, childId.trim)
  }

  def orbitCount(rawOrbits: Seq[String]) = {

    // map providing the parent of every child
    val parents = rawOrbits
      .map(parseOrbit)
      .map { case (parentId, childId) => (childId -> parentId) }
      .toMap

    @tailrec
    def orbitCount(node: String, count: Int): Int = {
      if (node == "COM") count
      else orbitCount(parents(node), count + 1)
    }

    // the set of all children is the set of all node (except "COM")
    val allNodes = parents.keys

    allNodes.toSeq.map(orbitCount(_, 0)).sum
  }

}
