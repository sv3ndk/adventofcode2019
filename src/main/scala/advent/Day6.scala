package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day6Parts extends App {
  Using(Source.fromFile("src/main/data/day6-orbits.txt")) {
    file =>
      val sumOrbitCount = solvePart1(file.getLines().toSeq)
      println(s"Advent of code 2019 - Day 6 / part 1: orbit count: $sumOrbitCount") // 294191
  }

  Using(Source.fromFile("src/main/data/day6-orbits.txt")) {
    file =>
      val orbitalTransfers = solvePart2(file.getLines().toSeq)
      println(s"Advent of code 2019 - Day 6 / part 2: orbit transfers: $orbitalTransfers") //
  }

  def solvePart1(fileContent: Seq[String]) = {
    val childToParentTree = Day6.childToParentTree(fileContent)
    Day6.orbitCount(childToParentTree)
  }

  def solvePart2(fileContent: Seq[String]) = {
    val childToParentTree = Day6.childToParentTree(fileContent)
    val path1 = Day6.pathToRoot(childToParentTree, "SAN")
    val path2 = Day6.pathToRoot(childToParentTree, "YOU")

    val commonAncestor = Day6.deepestCommonAncestor(childToParentTree, "SAN", "YOU")

    val hops1 = path1.takeWhile(_ != commonAncestor).size - 1
    val hops2 = path2.takeWhile(_ != commonAncestor).size - 1

    hops1 + hops2
  }
}

object Day6 {

  def parseOrbit(rawOrbit: String) = {
    val Seq(parentId, childId) = rawOrbit.split(("\\)")).toSeq
    (parentId.trim, childId.trim)
  }

  def childToParentTree(rawOrbits: Seq[String]): Map[String, String] =
    rawOrbits
      .map(parseOrbit)
      .map { case (parentId, childId) => (childId -> parentId) }
      .toMap

  def isRoot(node: String) = node == "COM"

  // finds the sequence of node from a the root to a child node
  def pathToRoot(childToParentTree: Map[String, String], node: String): Seq[String] =
    node +: (if (isRoot(node))
      Nil
    else pathToRoot(childToParentTree, childToParentTree(node)))

  def pathFromRoot(childToParentTree: Map[String, String], node: String): Seq[String] =
    pathToRoot(childToParentTree, node).reverse

  def deepestCommonAncestor(childToParentTree: Map[String, String], node1: String, node2: String) = {
    pathFromRoot(childToParentTree, node1)
      .zip(pathFromRoot(childToParentTree, node2))
      .takeWhile{ case(n1, n2) =>  n1 == n2 }
      .last
      ._1
  }

  // number of direct and indirect orbits around that node
  def orbitCount(childToParentTree: Map[String, String]) = {

    @tailrec
    def orbitCount(node: String, count: Int): Int = {
      if (isRoot(node)) count
      else orbitCount(childToParentTree(node), count + 1)
    }

    // the set of all children is the set of all node (except "COM")
    val allNodes = childToParentTree.keys

    allNodes.toSeq.map(orbitCount(_, 0)).sum
  }

}
