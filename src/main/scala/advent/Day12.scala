package advent

import advent.Day12.{Moon, Vect}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12Part1 extends App {
  val fileContent = Using(Source.fromFile("src/main/data/day12-moon-positions.txt")) { file => file.getLines().toList }
    .get

  val energy = Day12.runNSteps(1000, Day12.parseMoons(fileContent)).map(_.energy).sum
  println(s"Advent of code 2019 - Day 12 / part 1: energy: ${energy}") // 10055
}

object Day12Part2BruteForce extends App {
  val fileContent = Using(Source.fromFile("src/main/data/day12-moon-positions.txt")) { file => file.getLines().toList }
    .get

  // just running 10 million runs (without checks) takes about  30s => 5B would take 15000s ~ 4h
  // BUT running the search slows down to only 20000 runs searched in 30
  //  val repetition = Day12.bruteForceStep2(Day12.parseMoons(fileContent))

  // brute force approach, assuming we need to go back to the initial
  val repetition = Day12.bruteForceStep2bis(Day12.parseMoons(fileContent))

  // this never finishes..
  Day12.runNSteps(10000000, Day12.parseMoons(fileContent))
  println(s"Advent of code 2019 - Day 12 / part 2: steps before repetition: ${repetition}")
}

object Day12Part2 extends App {
  // ok, ok (thanks reddit channel) each dimension is independent, so if they are each cyclic, then we can just take
  // the LCM of the periods

  // also, each step has exactly one descendant and one parent => finding a loop is as simple as checking for
  // the first step

  // SO...
  val fileContent = Using(Source.fromFile("src/main/data/day12-moon-positions.txt")) { file => file.getLines().toList }
    .get

  val repetition = systemPeriod(fileContent)
  println(s"Advent of code 2019 - Day 12 / part 2: steps before repetition: ${repetition}") // 374307970285176


  def systemPeriod(rawMoons: List[String]): Long = {
    val List(m1, m2, m3, m4) = Day12.parseMoons(rawMoons).toList

    // looks at each Moon dimension independently
    def asDim(f: Vect => Int) = Set(
      Dim(m1.name, f(m1.position), f(m1.velocity)),
      Dim(m2.name, f(m2.position), f(m2.velocity)),
      Dim(m3.name, f(m3.position), f(m3.velocity)),
      Dim(m4.name, f(m4.position), f(m4.velocity))
    )

    val dimx = asDim(_.x)
    val dimy = asDim(_.y)
    val dimz = asDim(_.z)

    lcm(period(dimx), period(dimy), period(dimz))
  }


  def gcd(a: Long, b: Long): Long=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Long, b: Long): Long =(a*b).abs/gcd(a,b)

  def lcm(a: Long, b: Long, c:Long): Long = {
    println(a)
    println(b)
    println(c)
    lcm(lcm(a, b), c)
  }

  case class Dim(name: String, pos: Int, v: Int) {
    lazy val moved = copy(pos = pos + v)
  }

  def dimMap(dims: Set[Dim]): Map[String, Dim] = dims.map(d => d.name -> d).toMap

  def updateDimVelocities(d1: Dim, d2: Dim): (Dim, Dim) = {
    if (d1.pos == d2.pos) (d1, d2)
    else if (d1.pos > d2.pos) (d1.copy(v = d1.v - 1), d2.copy(v = d2.v + 1))
    else (d1.copy(v = d1.v + 1), d2.copy(v = d2.v - 1))
  }

  def updateDimOnce(dims: Set[Dim]): Set[Dim] =
    dims
      .map(_.name)
      .toList
      .combinations(2)
      .foldLeft(dimMap(dims)) {
        case (dimmap, List(name1, name2)) => {
          val (newDim1, newDim2) = updateDimVelocities(dimmap(name1), dimmap(name2))
          dimmap + (newDim1.name -> newDim1) + (newDim2.name -> newDim2)
        }
      }
      .values
      .map(_.moved)
      .toSet

  def period(startingPoint: Set[Dim]): Int = {
    @tailrec
    def loop(n: Int, prev: Set[Dim]): Int =
      if (prev == startingPoint) n
      else loop(n + 1, updateDimOnce(prev))

    loop(1, updateDimOnce(startingPoint))
  }

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

  def runNSteps(n: Int, moons: Set[Moon]): Set[Moon] = (1 to n).foldLeft(moons)((m, i) => {
    updateAllMoonsOnce(m)
  })

  def bruteForceStep2bis(first: Set[Moon]): Int = {
    def loop(n: Int, previous: Set[Moon]): Int = {
      if (first == previous) n
      else loop(n + 1, updateAllMoonsOnce(previous))
    }

    loop(1, updateAllMoonsOnce(first))
  }


}

// Discussion on velocity zero (I'm almost there...)

// (all the reasoning below assuming "current" means after both velocity and position update
// said otherwise, at the "current" state, the current position is about to be)

// prop [1]: each dimension is independent and can thus be computed independently
//  => that's easy to see in the algo description: each axis computation only depend on that axis


// prop [2]: from each step, there is only one next step
//  => that's also obvious from the algo: the computation is fully deterministic

// prop [3]: from each step, there is only one possible parent
//   3.1) given the current position and velocity, we know the previous position: that's a simple subtraction
//   3.2) given the previous position, I know the previous ordering, thus the velocity deltas, thus the previous velocity

// from [2] and [3]: if there is a loop, we can detect it by seeking a repetition of the 1rst state

// prop [4] if the sequence passes by velocity 0, it then goes backward to its previous positions in reverse order
//    assuming we have velocity 0 at point 11,
//      => so we have p11,v11=0
//    necessarily we have not moved since the previous step
//      => so we have p10,v10 => p11=p10,v11=0
//    if p11=p10, then necessarily v12=-v10 we applied the same delta (implied by the same position)
//      => so we have p10,v10 => p11=p10,v11=0 => p12,v12=-v10
//    but necessarily, if we applied the opposite velocity we go back in time
//      => so we have p8,v8 => p9,v9 => p10,v10 => p11=p10,v11=0 => p12=p9,v12=-v10 => p13=p8,v14=-v9 ...

//    so both position and velocity follow a symmetrical path, shifted by 1 position
//    also, we know the starting point of the simulation is such a point


// prop [5]: given that we start at velocity 0, if the sequence passes by 0, then we are sure to loop:
//   adding initial velocity 0 to the previous chain, we have:
//      => we have p1,v1=0 => p2,v2 ... => p10,v10 => p11=p10,v11=0 => p12=p9,v12=-v10 ...
//   so that at some point we reach p1 and v1=0, one after the other:
//      => we have p31=p1,v31=-v2, p32,v32=v1=0
//   and since v32=0 then p32=p31=p1
//      => p31=p1,v31=-v2, p32=p1,v32=v1=0
//    so we see that p31 is the same as p1,v1 => with [2] and [3], we are sure to loop

// => still, why are we guaranteed to reach that second velocity zero ?
