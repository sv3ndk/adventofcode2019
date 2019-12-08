package advent

import org.scalatest.{FlatSpec, Matchers}
import Day5.IntCodeComputer._

class Day5Spec extends FlatSpec with Matchers{

  "immediate parameter modes" should "read the buffer at the specified location" in {
    ParamMode.immediateMode(0)(Seq(1,2,3,4,5,6), 0) should be(2)
    ParamMode.immediateMode(0)(Seq(1,2,3,4,5,6), 1) should be(3)
    ParamMode.immediateMode(0)(Seq(1,2,3,4,5,6), 2) should be(4)
    ParamMode.immediateMode(0)(Seq(1,2,3,4,5,6), 3) should be(5)
    ParamMode.immediateMode(0)(Seq(1,2,3,4,5,6), 4) should be(6)
  }

  "position parameter modes" should "read the buffer at the specified location" in {
    ParamMode.positionMode(0)(Seq(1, 2, 3, 4, 5, 6), 0) should be(3)
    ParamMode.positionMode(0)(Seq(1, 2, 3, 4, 5, 6), 1) should be(4)
    ParamMode.positionMode(0)(Seq(1, 2, 3, 4, 5, 6), 3) should be(6)
  }

//  "parsing valid operations" should "work as expected" in {
//    Operation(99) should be (Operation.exit)
//    val a =Operation.Addition(ParamMode.positionMode(0), ParamMode.positionMode(1))
//
//    Operation(1) should be (Operation.Addition(ParamMode.positionMode(0), ParamMode.positionMode(1)))
//    Operation(2) should be (Operation.Multiplication(ParamMode.positionMode(0), ParamMode.positionMode(1)))
//    Operation(1002) should be (Operation.Multiplication(ParamMode.positionMode(0), ParamMode.immediateMode(1)))
//    Operation(1101) should be (Operation.Addition(ParamMode.immediateMode(0), ParamMode.immediateMode(1)))
//  }

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

  "checking if the input is 8" should "terminate ok (no clean output yet...)" in {
    runProgram(Seq (3,9,8,9,10,9,4,9,99,-1,8), 8).opPointer should be (8)
    runProgram(Seq (3,9,8,9,10,9,4,9,99,-1,8), 9).opPointer should be (8)
    runProgram(Seq (3,3,1108,-1,8,3,4,3,99), 8).opPointer should be (8)
    runProgram(Seq (3,3,1108,-1,8,3,4,3,99), 9).opPointer should be (8)
  }

  "checking if the input less than 8" should "terminate ok (no clean output yet...)" in {
    runProgram(Seq (3,9,7,9,10,9,4,9,99,-1,8 ), 4).opPointer should be (8)
    runProgram(Seq (3,9,7,9,10,9,4,9,99,-1,8 ), 9).opPointer should be (8)
    runProgram(Seq (3,3,1107,-1,8,3,4,3,99 ), 4).opPointer should be (8)
    runProgram(Seq (3,3,1107,-1,8,3,4,3,99 ), 9).opPointer should be (8)
  }

  "jumping" should "terminate ok (no clean output yet)" in {
    runProgram(Seq (3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 0).opPointer should be (11)
    runProgram(Seq (3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 5).opPointer should be (11)
    runProgram(Seq (3,3,1105,-1,9,1101,0,0,12,4,12,99,1,-1,0,1,9), 0).opPointer should be (11)
    runProgram(Seq (3,3,1105,-1,9,1101,0,0,12,4,12,99,1,-1,0,1,9), 1).opPointer should be (11)
  }

  "longer program" should "terminate ok (no clean output yet)"in {
    runProgram(Seq (3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), 1).opPointer should be (46)
    runProgram(Seq (3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), 8).opPointer should be (46)
    runProgram(Seq (3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), 10).opPointer should be (46)
  }


}