package advent

import scala.annotation.tailrec

object Day2Part1 extends App {
  val resultPart1 = Day2.run(12, 2)
  println(s"Advent of code 2019 - Day 2 / part 1: program after execution: ${resultPart1}")           // 9581917
}

object Day2Part2 extends App {
  val resultPart2 = Day2.inputFor(19690720)
  println(s"Advent of code 2019 - Day 2 / part 2: input noun and verb for 19690720: $resultPart2")    // (25, 5)
}

object Day2 {

  type Program = Seq[Int]

  def operation(pointer: Int, program: Program, op: (Int, Int) => Int): Program = {
    val _ :: offset1 :: offset2 :: resultOffset :: Nil = program.slice(pointer, pointer + 4)
    val updated = op(program(offset1), program(offset2))
    program.take(resultOffset) :+ updated :++ program.drop(resultOffset + 1)
  }

  val adder = operation(_, _, (a, b) => a + b)
  val multiplier = operation(_, _, (a, b) => a * b)

  @tailrec
  def runProgram(pointer: Int, program: Program): Program = {
    assert(pointer >= 0 && pointer < program.length)
    val operation = program(pointer)

    if (operation == 99) {
      program
    } else {

      val newProgram = program(pointer) match {
        case 1 => adder(pointer, program)
        case 2 => multiplier(pointer, program)
        case other => throw new RuntimeException(s"unknown operation: ${other} at position ${pointer} for ${program}")
      }

      runProgram(pointer + 4, newProgram)
    }
  }

  // execute the program of this exercise, with input parameters
  def run(noun: Int, verb: Int): Int = {
    val program = 1 :: noun :: verb :: List(3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0)
    val updatedProgram = runProgram(0, program)
    updatedProgram.head
  }

  // executes the program until it finds input that yields the specified input
  def inputFor(expectedOutput: Int): (Int, Int) = {

    val (noun, verb, result) = LazyList
      .tabulate(100, 100) { (noun, verb) => (noun, verb, run(noun,verb))}
      .flatten
      .filter{ case (noun, verb, result) => result == expectedOutput }
      .head

    (noun, verb)

  }
}
