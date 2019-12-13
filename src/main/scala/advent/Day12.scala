package advent

import scala.io.Source
import scala.util.Using

object Day12Part1 extends App {
  val fileContent = Using(Source.fromFile("src/main/data/day12-moon-positions.txt")) { file => file.getLines().toList }
    .get

  val energy = Day12.runNSteps(1000, Day12.parseMoons(fileContent)).map(_.energy).sum
  println(s"Advent of code 2019 - Day 12 / part 1: energy: ${energy}") // 10055
}

object Day12 {

  def parseMoons(rawMoons: List[String]): Set[Moon] =
    rawMoons.zip(List("Io", "Europa", "Ganymede", "Callisto"))
      .map { case (coord, name) => Moon(name, coord) }
      .toSet

  case class Vect(x: Int, y: Int, z: Int) {
    def +(other: Vect) = Vect(x + other.x, y + other.y, z + other.z)
  }

  object Vect {
    val zero = Vect(0, 0, 0)
  }

  case class Moon(name: String, position: Vect, velocity: Vect) {
    lazy val moved = copy(position = position + velocity)

    lazy val pot = position.x.abs + position.y.abs + position.z.abs
    lazy val kin = velocity.x.abs + velocity.y.abs + velocity.z.abs
    lazy val energy = pot * kin
  }

  object Moon {
    // <x=3, y=5, z=-1>
    def apply(name: String, rawMoon: String): Moon = {
      val x :: y :: z :: Nil = rawMoon.trim
        .replace("<", "")
        .replace(">", "")
        .replace(" ", "")
        .split(",")
        .map(_.split("=")(1).toInt).toList

      Moon(name, x, y, z)
    }

    def apply(name: String, x: Int, y: Int, z: Int): Moon = Moon(name, Vect(x, y, z), Vect.zero)

  }

  type VelocityUpdate = Int => Int

  object VelocityUpdate {
    val sameSame: VelocityUpdate = p => p
    val increase: VelocityUpdate = p => p + 1
    val decrease: VelocityUpdate = p => p - 1
  }

  // root algo: provide 2 velocity update function based on the both positions
  def velocityUpdate(pos1: Int, pos2: Int): (Int => Int, Int => Int) =
    if (pos1 == pos2) (VelocityUpdate.sameSame, VelocityUpdate.sameSame)
    else if (pos1 < pos2) (VelocityUpdate.increase, VelocityUpdate.decrease)
    else (VelocityUpdate.decrease, VelocityUpdate.increase)

  def updateMoonVelocities(moon1: Moon, moon2: Moon): (Moon, Moon) = {
    val (xup1, xup2) = velocityUpdate(moon1.position.x, moon2.position.x)
    val (yup1, yup2) = velocityUpdate(moon1.position.y, moon2.position.y)
    val (zup1, zup2) = velocityUpdate(moon1.position.z, moon2.position.z)
    (
      moon1.copy(velocity = Vect(xup1(moon1.velocity.x), yup1(moon1.velocity.y), zup1(moon1.velocity.z))),
      moon2.copy(velocity = Vect(xup2(moon2.velocity.x), yup2(moon2.velocity.y), zup2(moon2.velocity.z)))
    )
  }

  def moonmap(moons: Set[Moon]): Map[String, Moon] = moons.map(moon => moon.name -> moon).toMap

  def updateAllMoonsOnce(moons: Set[Moon]): Set[Moon] =
    moons
      .map(_.name)
      .toList
      .combinations(2)
      .foldLeft(moonmap(moons)) {
        case (mm, List(name1, name2)) => {
          val (newMoon1, newMoon2) = updateMoonVelocities(mm(name1), mm(name2))
          mm + (newMoon1.name -> newMoon1) + (newMoon2.name -> newMoon2)
        }
      }
      .values
      .map(_.moved)
      .toSet

  def runNSteps(n: Int, moons: Set[Moon]): Set[Moon] = (1 to n).foldLeft(moons)((m, _) => updateAllMoonsOnce(m))

}
