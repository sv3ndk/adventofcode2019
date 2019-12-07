package advent

import scala.annotation.tailrec

object Day2 extends App {

  println("Advent of code 2019 - Day 2")

//  val program = Seq(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0)
  val program = Seq(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0)

  val result = Day2Solution.run(0, program)
  println(s"program after execution: ${result}")

}

object Day2Solution {

  type Program = Seq[Int]

  def operation(position: Int, program: Program, op: (Int, Int) => Int): Program = {
    val _ :: offset1 :: offset2 :: resultOffset :: Nil = program.slice(position, position + 4)
    val updated = op(program(offset1), program(offset2))
    program.take(resultOffset) :+ updated :++ program.drop(resultOffset + 1)
  }

  val adder = operation(_, _, (a, b) => a + b)
  val multiplier = operation(_, _, (a, b) => a * b)

  @tailrec
  def run(position: Int, program: Program): Program = {
    assert(position >= 0 && position < program.length)
    val operation = program(position)

    println(s"operation ${operation} at position ${position}, program ${program}")

    if (operation == 99) {
      program
    } else {

      val newProgram = program(position) match {
        case 1 => adder(position, program)
        case 2 => multiplier(position, program)
        case other => throw new RuntimeException(s"unknown operation: ${other} at position ${position} for ${program}")
      }

      run(position + 4, newProgram)
    }
  }
}
