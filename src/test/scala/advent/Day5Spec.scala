package advent

import org.scalatest.{FlatSpec, Matchers}
import Day5.IntCodeComputer._

class Day5Spec extends FlatSpec with Matchers{

  "immediate parameter modes" should "read the buffer at the specified location" in {
    ParamMode.immediateMode(Seq(1,2,3,4,5,6), 0) should be(1)
    ParamMode.immediateMode(Seq(1,2,3,4,5,6), 1) should be(2)
    ParamMode.immediateMode(Seq(1,2,3,4,5,6), 2) should be(3)
    ParamMode.immediateMode(Seq(1,2,3,4,5,6), 3) should be(4)
    ParamMode.immediateMode(Seq(1,2,3,4,5,6), 4) should be(5)
  }

  "position parameter modes" should "read the buffer at the specified location" in {
    ParamMode.positionMode(Seq(1, 2, 3, 4, 5, 6), 0) should be(2)
    ParamMode.positionMode(Seq(1, 2, 3, 4, 5, 6), 1) should be(3)
    ParamMode.positionMode(Seq(1, 2, 3, 4, 5, 6), 3) should be(5)
  }

  "parsing valid operations" should "work as expected" in {
    Operation(99) should be (Operation.exit)
    Operation(1) should be (Operation.Addition(ParamMode.positionMode, ParamMode.positionMode))
    Operation(2) should be (Operation.Multiplication(ParamMode.positionMode, ParamMode.positionMode))
    Operation(1002) should be (Operation.Multiplication(ParamMode.positionMode, ParamMode.immediateMode))
    Operation(1101) should be (Operation.Addition(ParamMode.immediateMode, ParamMode.immediateMode))
  }

  "example program from day 2" should "still work here" in {
    runProgram(Seq (1,0,0,0,99), 0).program should be (Seq(2,0,0,0,99))
  }

  "example program 2 from day 2" should "still work here" in {
    runProgram(Seq (2,3,0,3,99), 0).program should be (Seq(2,3,0,6,99))
  }

  "example program 3 from day 2" should "still work here" in {
    runProgram(Seq(2,4,4,5,99,0), 0).program should be (Seq(2,4,4,5,99,9801))
  }

  "example program 4 from day 2" should "still work here" in {
    runProgram(Seq (1,1,1,4,99,5,6,0,99), 0).program should be (Seq(30,1,1,4,2,5,6,0,99))
  }


}