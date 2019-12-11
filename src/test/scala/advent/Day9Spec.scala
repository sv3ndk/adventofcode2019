package advent

import advent.Day9.IntCodeComputer
import org.scalatest.{FlatSpec, Matchers}

class Day9Spec extends FlatSpec with Matchers {

  "(from day5) example program from day 2" should "still work here" in {
    new IntCodeComputer(Seq (1,0,0,0,99).map(_.toLong)).start( 0).program.take(5) should be (Seq(2,0,0,0,99))
  }

  "(from day5) example program 2 from day 2" should "still work here" in {
    new IntCodeComputer(Seq (2,3,0,3,99).map(_.toLong)).start( 0).program.take(5) should be (Seq(2,3,0,6,99))
  }

  "(from day5) example program 3 from day 2" should "still work here" in {
    new IntCodeComputer(Seq(2,4,4,5,99,0).map(_.toLong)).start( 0).program.take(6) should be (Seq(2,4,4,5,99,9801))
  }

  "(from day5) example program 4 from day 2" should "still work here" in {
    new IntCodeComputer(Seq (1,1,1,4,99,5,6,0,99).map(_.toLong)).start( 0).program.take(9) should be (Seq(30,1,1,4,2,5,6,0,99))
  }

  "(from day5) consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8).map(_.toLong)
    new IntCodeComputer(program).start( 8).output should be (Seq(1))
    new IntCodeComputer(program).start( 3).output should be (Seq(0))
    new IntCodeComputer(program).start( 13).output should be (Seq(0))
  }

  "(from day5) consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8).map(_.toLong)
    new IntCodeComputer(program).start( 1).output should be (Seq(1))
    new IntCodeComputer(program).start( 3).output should be (Seq(1))
    new IntCodeComputer(program).start( 8).output should be (Seq(0))
    new IntCodeComputer(program).start( 18).output should be (Seq(0))
  }

  "(from day5) Using immediate mode, consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1108,-1,8,3,4,3,99).map(_.toLong)
    new IntCodeComputer(program).start( 1).output should be(Seq(0))
    new IntCodeComputer(program).start( 8).output should be(Seq(1))
    new IntCodeComputer(program).start( 10).output should be(Seq(0))
  }

  "(from day5) Using immediate mode, consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1107,-1,8,3,4,3,99).map(_.toLong)
    new IntCodeComputer(program).start(8).output should be(Seq(0))
    new IntCodeComputer(program).start( 1).output should be(Seq(1))
    new IntCodeComputer(program).start(8).output should be(Seq(0))
  }

  "(from day5) using position mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9).map(_.toLong)
    new IntCodeComputer(program).start( -10).output should be (Seq(1))
    new IntCodeComputer(program).start( 0).output should be (Seq(0))
    new IntCodeComputer(program).start( 10).output should be (Seq(1))
  }

  "(from day5) using immediate mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,3,1105,-1,9,1101,0,0,12,4,12,99,1).map(_.toLong)
    new IntCodeComputer(program).start( 0).output should be (Seq(0))
    new IntCodeComputer(program).start( 10).output should be (Seq(1))
    new IntCodeComputer(program).start( -10).output should be (Seq(1))
  }

  "(from day5) larger example" should "output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8." in {
    val program = Seq(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99).map(_.toLong)

    new IntCodeComputer(program).start( 0).output should be(Seq(999))
    new IntCodeComputer(program).start( -890).output should be(Seq(999))
    new IntCodeComputer(program).start( 7).output should be(Seq(999))

    new IntCodeComputer(program).start( 8).output should be(Seq(1000))

    new IntCodeComputer(program).start( 9).output should be(Seq(1001))
    new IntCodeComputer(program).start( 19).output should be(Seq(1001))
  }

  "example of self copy program" should "produce a copy of itself" in {
    val program = Seq (109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(_.toLong)
    new IntCodeComputer(program).start(Seq.empty).output should be (program)
  }

  "1102,34915192,34915192,7,4,7,99,0" should " should output a 16-digit number" in {
    val program = Seq (1102,34915192,34915192,7,4,7,99,0).map(_.toLong)
    new IntCodeComputer(program).start(Seq.empty).output.head.toString.length should be (16)
  }

  "104,1125899906842624,99" should "output the large number in the middle" in {
    val program = Seq (104L,1125899906842624L,99L)
    new IntCodeComputer(program).start(Seq.empty).output.head should be (1125899906842624L)
  }

}
