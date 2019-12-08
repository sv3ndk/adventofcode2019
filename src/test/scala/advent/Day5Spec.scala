package advent

import org.scalatest.{FlatSpec, Matchers}
import Day5.IntCodeComputer._

class Day5Spec extends FlatSpec with Matchers{

  "immediate parameter modes" should "read the buffer at the specified location" in {
    val prog = new State(Seq(1,2,3,4,5,6),12345)

    Param.immediateMode(0)(prog) should be(2)
    Param.immediateMode(1)(prog) should be(3)
    Param.immediateMode(2)(prog) should be(4)

    val prog2 = prog.forward(4)
    Param.immediateMode(0)(prog2) should be(6)
  }

  "position parameter modes" should "read the buffer at the specified location" in {
    val prog = new State(Seq(1,2,3,4,5,6),12345)
    Param.positionMode(0)(prog) should be(3)
    Param.positionMode(1)(prog) should be(4)
    Param.positionMode(2)(prog) should be(5)
  }

  "example program from day 2" should "still work here" in {
    applyProgram(Seq (1,0,0,0,99), 0).program should be (Seq(2,0,0,0,99))
  }

  "example program 2 from day 2" should "still work here" in {
    applyProgram(Seq (2,3,0,3,99), 0).program should be (Seq(2,3,0,6,99))
  }

  "example program 3 from day 2" should "still work here" in {
    applyProgram(Seq(2,4,4,5,99,0), 0).program should be (Seq(2,4,4,5,99,9801))
  }

  "example program 4 from day 2" should "still work here" in {
    applyProgram(Seq (1,1,1,4,99,5,6,0,99), 0).program should be (Seq(30,1,1,4,2,5,6,0,99))
  }

  "consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
    applyProgram(program, 8).output should be (Seq(1))
    applyProgram(program, 3).output should be (Seq(0))
    applyProgram(program, 13).output should be (Seq(0))
  }

  "consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
    applyProgram(program, 1).output should be (Seq(1))
    applyProgram(program, 3).output should be (Seq(1))
    applyProgram(program, 8).output should be (Seq(0))
    applyProgram(program, 18).output should be (Seq(0))
  }

  "Using immediate mode, consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1108,-1,8,3,4,3,99)
    applyProgram(program, 1).output should be(Seq(0))
    applyProgram(program, 8).output should be(Seq(1))
    applyProgram(program, 10).output should be(Seq(0))
  }

  "Using immediate mode, consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1107,-1,8,3,4,3,99)
    applyProgram(program, 1).output should be(Seq(1))
    applyProgram(program, 8).output should be(Seq(0))
    applyProgram(program, 8).output should be(Seq(0))
  }

  "using position mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    applyProgram(program , 0).output should be (Seq(0))
    applyProgram(program , 10).output should be (Seq(1))
    applyProgram(program , -10).output should be (Seq(1))
  }

  "using immediate mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    applyProgram(program , 0).output should be (Seq(0))
    applyProgram(program , 10).output should be (Seq(1))
      applyProgram(program , -10).output should be (Seq(1))
    }

  "larger example" should "output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8." in {
    val program = Seq(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

    applyProgram(program, 0).output should be(Seq(999))
    applyProgram(program, -890).output should be(Seq(999))
    applyProgram(program, 7).output should be(Seq(999))

    applyProgram(program, 8).output should be(Seq(1000))

    applyProgram(program, 9).output should be(Seq(1001))
    applyProgram(program, 19).output should be(Seq(1001))
  }


}