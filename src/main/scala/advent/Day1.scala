package advent

import math._
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day1 extends App {

  println("Advent of code 2019 - Day 1")

  Using(Source.fromFile("src/main/data/day1-module-masses.txt")) {
    file => fuelFor(file.getLines().map(_.toInt))
  }.foreach(result => println(s"total fuel quantity (part 1): ${result}"))

  Using(Source.fromFile("src/main/data/day1-module-masses.txt")) {
    file => totalFuelFor(file.getLines().map(_.toInt))
  }.foreach(result => println(s"total fuel quantity (part 2): ${result}"))

  // mass of fuel to carry that module
  def fuelFor(module: Int): Int = floor(module / 3).toInt - 2

  // mass of fuel to carry all those modules, without taking into account the fuel of fuel, i.e. part 1
  def fuelFor(modules: Iterator[Int]): Int = modules.map(fuelFor).sum

  // total mass of fuel of that module, each with the fuel for their fuel (computed individually), i.e. part 2
  def totalFuelFor(module: Int): Int = {

    @tailrec
    def totalFuel(fuel: Int, cumulativeFuel: Int): Int = {
      val moreFuel = fuelFor(fuel)
      if (moreFuel > 0) totalFuel(moreFuel, fuel + cumulativeFuel) else fuel + cumulativeFuel
    }

    totalFuel(fuelFor(module), 0)
  }

  // total mass of fuel of all those cargo, each with the fuel for their fuel (computed individually)
  def totalFuelFor(modules: Iterator[Int]): Int = {
    modules.map(totalFuelFor).sum
  }
}
