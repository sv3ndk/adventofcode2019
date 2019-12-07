package advent

import org.scalatest._

class Day2Spec extends FlatSpec with Matchers {

  "example of adder" should "match" in {
    val program = Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
    val resultOneStep = Seq(1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    Day2Solution.adder(0, program) should be(resultOneStep)
  }

  "example of multiplier" should "match" in {
    val program = Seq(1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    val resultOneStep = Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    Day2Solution.multiplier(4, program) should be(resultOneStep)
  }

  "example program" should "match" in {
    Day2Solution.runProgram(0, Seq (1,0,0,0,99)) should be (Seq(2,0,0,0,99))
  }

  "example program2" should "match" in {
    Day2Solution.runProgram(0, Seq (2,3,0,3,99)) should be (Seq(2,3,0,6,99))
  }

  "example program3" should "match" in {
    Day2Solution.runProgram(0, Seq(2,4,4,5,99,0)) should be (Seq(2,4,4,5,99,9801))
  }

  "example program4" should "match" in {
    Day2Solution.runProgram(0, Seq (1,1,1,4,99,5,6,0,99)) should be (Seq(30,1,1,4,2,5,6,0,99))
  }

  "running part 2 solution with part 1 input" should " yield the same result" in {
    Day2Solution.run(12, 2) should be (9581917)
  }

}
