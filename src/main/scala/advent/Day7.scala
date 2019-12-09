package advent


import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day7Part extends App {

  val computer = Day7.IntCodeComputer.parseProgram("src/main/data/day7-amplifier-program.txt")
  val (phaseSettings, power)  = optimalPhases(computer)
  println(s"Advent of code 2019 - Day 7 / part 1: $power with settingsr $phaseSettings")

  // runs that program as many times as there are phase settings, chaining each output to as input for the next amp
  def chainAmplifiers(amplifierControlSoftware: Seq[Int], phaseSettings: Seq[Int], firstInput: Int): Int = {
    val intComputer = new Day7.IntCodeComputer(amplifierControlSoftware)
    phaseSettings.foldLeft(firstInput) { (output, phase) => intComputer.run(phase, output).head }
  }

  // finds the set of phase settings that yield the best thrust power
  def optimalPhases(amplifierControlSoftware: Seq[Int]): (Seq[Int], Int) = {
    List(0, 1, 2, 3, 4)
      .permutations
      .map(phaseSettings => (phaseSettings, chainAmplifiers(amplifierControlSoftware, phaseSettings, 0)))
      .reduce((run1, run2) => if (run1._2 > run2._2) run1 else run2)
  }

}


object Day7 {

  import advent.Day7.IntCodeComputer.State

  /**
   * 3rd iteration on the initCode computer:
   *
   * this one accepts a sequence of inputs and provides a Seq of outputs. When an input is read by the program, it is
   * un-piled from the stack and no longer available
   */
  class IntCodeComputer(startProgram: Seq[Int]) {

    /**
     * Main entry point of this IntCode computer: execute this program with this input and provides the output as
     * return value
     */
    def run(input: Int*): Seq[Int] = {
      //      println(s"running with $input")
      doRun(input: _*).output
    }

    // verbose output version, for UTs
    def doRun(input: Int*): State = loop(new State(startProgram, input.toList))

    @tailrec private def loop(state: State): State = if (!state.isRunning) state else loop(state.step)
  }

  object IntCodeComputer {

    def parseProgram(programLocation: String): Seq[Int] = {
      Using(Source.fromFile(programLocation)) { file => file.getLines().toSeq.head }
        .get
        .split(",").map(Integer.parseInt)
        .toSeq
    }

    case class State(program: Seq[Int], opPointer: Int, input: Seq[Int], output: Seq[Int], isRunning: Boolean) {

      def this(program: Seq[Int], input: Seq[Int]) = this(program, 0, input, Nil, true)

      // move the operation pointer to the specified location
      def jumpTo(newOpPointer: Int): State = copy(opPointer = newOpPointer)

      // moves the operation pointer forward
      def forward(size: Int): State = jumpTo(opPointer + size)

      // adds this to the program output
      def addOutput(moreOutPut: Int) = copy(output = moreOutPut +: output)

      lazy val stopped = copy(isRunning = false)

      lazy val currentOpCode: Int = program(opPointer)

      // execute the operation at the current op pointer
      lazy val step: State = Operation(currentOpCode)(this)

      // updates the program by writing the specified value
      // writeParamPosition determines the input param containing the address where to write to
      // (cf "Parameters that an instruction writes to will never be in immediate mode.")
      def write(writeParamPosition: Int, value: Int): State = {
        val writePointer = Param.immediateMode(writeParamPosition)(this)
        copy(program = program.take(writePointer) :+ value :++ program.drop(writePointer + 1))
      }

      // reads one input from the program (which is then removed and no longer available later)
      lazy val readInput = {
        assert(input.nonEmpty, "cannot access input: Empty (not enough argument?)")
        (input.head, copy(input = input.tail))
      }
    }

    // accessor of a function parameter in the program
    type Param = State => Int

    object Param {

      // param order: position among the parameters of the function this parameter belongs to, starting at 0
      def immediateMode(paramPosition: Int): Param = {
        case State(program, pointer, _, _, _) => program(pointer + paramPosition + 1)
      }

      def positionMode(paramPosition: Int): Param = {
        case State(program, pointer, _, _, _) => program(program(pointer + paramPosition + 1))
      }

      def apply(encodedMode: Char, paramPosition: Int): Param =
        encodedMode match {
          case '0' => positionMode(paramPosition)
          case '1' => immediateMode(paramPosition)
          case _ => throw new RuntimeException(s"unknown parameter mode: $encodedMode")
        }
    }

    // step from one program state to another
    type Operation = State => State

    object Operation {

      // looks up 2 values and writes their sum at the specified location
      def addition(val1: Param, val2: Param): Operation =
        state => state.write(2, val1(state) + val2(state)).forward(4)

      // looks up 2 values and writes their product at the specified location
      def multiplication(val1: Param, val2: Param): Operation =
        state => state.write(2, val1(state) * val2(state)).forward(4)

      // consumes one input and writes it in the program at the specified location
      val input: Operation = (state: State) => {
        val (consumedInput, updatedState) = state.readInput
        updatedState.write(0, consumedInput).forward(2)
      }

      // looks up a value (by ref or value) and writes it in output
      def output(param: Param): Operation = state => state.addOutput(param(state)).forward(2)

      def jumpIfTrue(val1: Param, val2: Param): Operation =
        state => if (val1(state) != 0) state.jumpTo(val2(state)) else state.forward(3)

      def jumpIfFalse(val1: Param, val2: Param): Operation =
        state => if (val1(state) == 0) state.jumpTo(val2(state)) else state.forward(3)

      def lessThan(val1: Param, val2: Param): Operation =
        state => state.write(2, if (val1(state) < val2(state)) 1 else 0).forward(4)

      def areParamsEqual(val1: Param, val2: Param): Operation =
        state => state.write(2, if (val1(state) == val2(state)) 1 else 0).forward(4)

      val exit: Operation = (state: State) => state.stopped

      // determines the Operation c
      def apply(operation: Int): Operation = {

        val operationStringRev = operation.toString.reverse

        // param modes know how to read a param: immediate mode vs position mode + where the parameter is in the stack
        val paramModes = operationStringRev.drop(2).padTo(3, '0')
          .zipWithIndex
          .map { case (mode, paramPosition) => Param.apply(mode, paramPosition) }

        operationStringRev.take(2).replace("0", "") match {
          case "1" => addition(paramModes(0), paramModes(1))
          case "2" => multiplication(paramModes(0), paramModes(1))
          case "3" => input
          case "4" => output(paramModes(0))
          case "5" => jumpIfTrue(paramModes(0), paramModes(1))
          case "6" => jumpIfFalse(paramModes(0), paramModes(1))
          case "7" => lessThan(paramModes(0), paramModes(1))
          case "8" => areParamsEqual(paramModes(0), paramModes(1))
          case "99" => exit
          case _ => throw new RuntimeException(s"unrecognized operation: ${operation}")
        }
      }
    }

  }

}
