package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day5Part1 extends App {
  println(s"Advent of code 2019 - Day 5 / part 1:")
  Day5.IntCodeComputer.execute("src/main/data/day5-program.txt", 1) // 8332629

  println(s"Advent of code 2019 - Day 5 / part 2:")
  Day5.IntCodeComputer.execute("src/main/data/day5-program.txt", 5) // 8805067
}

object Day5 {

  object IntCodeComputer {

    case class State(program: Seq[Int], opPointer: Int, input: Int, output: Seq[Int], isRunning: Boolean) {

      def this(program: Seq[Int], input: Int) = this(program, 0, input, Nil, true)

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

      // updates the program by writing the specified values based on writeParamPosition.
      // writeParamPosition determines the input param containing the address where to write to
      // (cf "Parameters that an instruction writes to will never be in immediate mode.")
      def write(writeParamPosition: Int, value: Int): State = {
        val writePointer = Param.immediateMode(writeParamPosition)(this)
        copy(program = program.take(writePointer) :+ value :++ program.drop(writePointer + 1))
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

      // writes the program input at the specified location
      val input: Operation = (state: State) => state.write(0, state.input).forward(2)

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

    /**
     * Entry point of this IntCode computer: execute this program with this input
     */
    def applyProgram(program: Seq[Int], input: Int): State = {

      @tailrec def loop(state: State): State = if (!state.isRunning) state else loop(state.step)

      loop(new State(program, input))
    }

    def parseProgram(line: String) = line.split(",").map(Integer.parseInt)

    def execute(filepath: String, input: Int): Unit = {
      Using(Source.fromFile(filepath)) {
        file =>
          Day5.IntCodeComputer
            .applyProgram(parseProgram(file.getLines().next()), input)
            .output
            .foreach(o => println(s"output: $o"))
      }
    }
  }

}
