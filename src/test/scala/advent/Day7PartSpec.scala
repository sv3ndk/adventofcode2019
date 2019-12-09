package advent

import org.scalatest.{FlatSpec, Matchers}
import Day7.IntCodeComputer.{State, _}
import advent.Day7.IntCodeComputer

class Day7PartSpec extends FlatSpec with Matchers {

  "(from day5) immediate parameter modes" should "read the buffer at the specified location" in {
    val prog = new State(Seq(1,2,3,4,5,6),List(12345), "rst")

    Param.immediateMode(0)(prog) should be(2)
    Param.immediateMode(1)(prog) should be(3)
    Param.immediateMode(2)(prog) should be(4)

    val prog2 = prog.forward(4)
    Param.immediateMode(0)(prog2) should be(6)
  }

  "(from day5) position parameter modes" should "read the buffer at the specified location" in {
    val prog = new State(Seq(1,2,3,4,5,6),List(12345), "")
    Param.positionMode(0)(prog) should be(3)
    Param.positionMode(1)(prog) should be(4)
    Param.positionMode(2)(prog) should be(5)
  }

  "(from day5) example program from day 2" should "still work here" in {
    new IntCodeComputer(Seq (1,0,0,0,99)).start( List(0)).program should be (Seq(2,0,0,0,99))
  }

  "(from day5) example program 2 from day 2" should "still work here" in {
    new IntCodeComputer(Seq (2,3,0,3,99)).start( List(0)).program should be (Seq(2,3,0,6,99))
  }

  "(from day5) example program 3 from day 2" should "still work here" in {
    new IntCodeComputer(Seq(2,4,4,5,99,0)).start( List(0)).program should be (Seq(2,4,4,5,99,9801))
  }

  "(from day5) example program 4 from day 2" should "still work here" in {
    new IntCodeComputer(Seq (1,1,1,4,99,5,6,0,99)).start( List(0)).program should be (Seq(30,1,1,4,2,5,6,0,99))
  }


  "(from day5) consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
    new IntCodeComputer(program).start( 8).output should be (Seq(1))
    new IntCodeComputer(program).start( 3).output should be (Seq(0))
    new IntCodeComputer(program).start( 13).output should be (Seq(0))
  }

  "(from day5) consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
    new IntCodeComputer(program).start( 1).output should be (Seq(1))
    new IntCodeComputer(program).start( 3).output should be (Seq(1))
    new IntCodeComputer(program).start( 8).output should be (Seq(0))
    new IntCodeComputer(program).start( 18).output should be (Seq(0))
  }

  "(from day5) Using immediate mode, consider whether the input is equal to 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1108,-1,8,3,4,3,99)
    new IntCodeComputer(program).start( 1).output should be(Seq(0))
    new IntCodeComputer(program).start( 8).output should be(Seq(1))
    new IntCodeComputer(program).start( 10).output should be(Seq(0))
  }

  "(from day5) Using immediate mode, consider whether the input is less than 8" should "output 1 (if it is) or 0 (if it is not)." in {
    val program = Seq(3,3,1107,-1,8,3,4,3,99)
    new IntCodeComputer(program).start(8).output should be(Seq(0))
    new IntCodeComputer(program).start( 1).output should be(Seq(1))
    new IntCodeComputer(program).start(8).output should be(Seq(0))
  }

  "(from day5) using position mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    new IntCodeComputer(program).start( -10).output should be (Seq(1))
    new IntCodeComputer(program).start( 0).output should be (Seq(0))
    new IntCodeComputer(program).start( 10).output should be (Seq(1))
  }

  "(from day5) using immediate mode, some jump tests that take an input, then" should "output 0 if the input was zero or 1 if the input was non-zero:" in {
    val program = Seq(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    new IntCodeComputer(program).start( 0).output should be (Seq(0))
    new IntCodeComputer(program).start( 10).output should be (Seq(1))
    new IntCodeComputer(program).start( -10).output should be (Seq(1))
  }

  "(from day5) larger example" should "output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8." in {
    val program = Seq(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

    new IntCodeComputer(program).start( 0).output should be(Seq(999))
    new IntCodeComputer(program).start( -890).output should be(Seq(999))
    new IntCodeComputer(program).start( 7).output should be(Seq(999))

    new IntCodeComputer(program).start( 8).output should be(Seq(1000))

    new IntCodeComputer(program).start( 9).output should be(Seq(1001))
    new IntCodeComputer(program).start( 19).output should be(Seq(1001))
  }

  "1rst example phase execution" should "yield the specified power" in {
    Day7Part1.chainAmplifiers(
      Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0),
      Seq(4,3,2,1,0)) should be (43210)
  }

  "1rst optimal phase settings of 1rst example" should "match" in {
    Day7Part1.optimalPhases(Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)) should be (Seq(4,3,2,1,0), 43210)
  }

  "2nd example phase execution" should "yield the specified power" in {
    Day7Part1.chainAmplifiers(
      Seq(3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0),
      Seq(0,1,2,3,4)) should be (54321)
  }

  "optimal phase settings of 2nd example" should "match" in {
    Day7Part1.optimalPhases(
      Seq(3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0)) should be (Seq(0,1,2,3,4), 54321)
  }

  "3rd example phase execution" should "yield the specified power" in {
    Day7Part1.chainAmplifiers(
      Seq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0),
      Seq(1,0,4,3,2)) should be (65210)
  }

  "optimal phase settings of 3nd example" should "match" in {
    Day7Part1.optimalPhases(
      Seq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
    ) should be (Seq(1,0,4,3,2), 65210)
  }

  "part 2 1rst example chain" should "yield the specified power" in {
    Day7Part2.loopingAmplifiersChain(
      Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5),
      Seq(9,8,7,6,5)) should be (139629729)
  }

  "optimal phase settings of part 2 1rst example" should "match" in {
    Day7Part2.optimalPhases(
      Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    ) should be (Seq(9,8,7,6,5), 139629729)
  }

  "part 2 2nd example chain" should "yield the specified power" in {
    Day7Part2.loopingAmplifiersChain(
      Seq(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10),
      Seq(9,7,8,5,6)) should be (18216)
  }

  "optimal phase settings of part 2 2nd example" should "match" in {
    Day7Part2.optimalPhases(
      Seq(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
    ) should be (Seq(9,7,8,5,6), 18216)
  }

}
