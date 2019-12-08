package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day5Parts extends App {

  Using(Source.fromFile("src/main/data/day5-program.txt")) {
    file => Day5.IntCodeComputer.runProgram(
      parseProgram(file.getLines().next()),
      1)
  }

  def parseProgram(line: String) = line.split(",").map(Integer.parseInt)
}


object Day5 {

  object IntCodeComputer {

    // accessor of a parameter in the program
    type ParamMode = (Seq[Int], Int) => Int

    object ParamMode {
      val immediateMode: ParamMode = (program, pointer) => program(pointer)
      val positionMode: ParamMode = (program, pointer) => program(program(pointer))

      def apply(encodedMode: Char): ParamMode =
        encodedMode match {
          case '0' => positionMode
          case '1' => immediateMode
          case _ => throw new RuntimeException(s"unknown parameter mode: $encodedMode")
        }
    }

    case class InputState(program: Seq[Int], opPointer: Int, input: Int)

    case class OutputState(program: Seq[Int], opPointer: Int)

    // updates the program by writing that value at that location
    // pointerPointer is the address containing the address where to write
    // (cf "Parameters that an instruction writes to will never be in immediate mode.")
    def write(program: Seq[Int], pointerPointer: Int, value: Int): Seq[Int] = {
      val pointer = ParamMode.immediateMode(program, pointerPointer)
      program.take(pointer) :+ value :++ program.drop(pointer + 1)
    }

    // step from one program state to another, having access to program input optionally able to specify the output
    type Operation = InputState => Option[OutputState]

    object Operation {

      // looks up 2 values (by ref or value) and writes their sum at the specified location (always in position mode)
      case class Addition(term1: ParamMode, term2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          val sum = term1(state.program, state.opPointer + 1) + term2(state.program, state.opPointer + 2)
          Some(OutputState(
            write(state.program, state.opPointer + 3, sum),
            state.opPointer + 4))
        }
      }

      // looks up 2 values (by ref or value) and writes their product at the specified location (always in position mode)
      case class Multiplication(factor1: ParamMode, factor2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          val product = factor1(state.program, state.opPointer + 1) * factor2(state.program, state.opPointer + 2)
          Some(OutputState(
            write(state.program, state.opPointer + 3, product),
            state.opPointer + 4))
        }
      }

      // writes the program input  at the specified location (always in position mode)
      val input: Operation = {
        case (InputState(program, opPointer, input)) =>
          Some(OutputState(write(program, opPointer + 1, input), opPointer + 2))
      }

      // looks up a value (by ref or value) and writes it in output
      case class Output(output: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          println(s"[OUTPUT]: ${output(state.program, state.opPointer + 1)}")
          Some(OutputState(state.program, state.opPointer + 2))
        }
      }

      val exit: Operation = { _ => None }

      // parses a string into the corresponding operation
      def apply(operation: Int): Operation = {
        val operationString = operation.toString.reverse
        assert(operationString.length >= 1, s"invalid operation: ${operation}, must be a least 2 char long")

        val paramModes = operationString.drop(2).padTo(3, '0').map(ParamMode.apply)

        operationString.take(2).replace("0", "") match {
          case "1" => Addition(paramModes(0), paramModes(1))
          case "2" => Multiplication(paramModes(0), paramModes(1))
          case "3" => input
          case "4" => Output(paramModes(0))
          case "99" => exit
          case _ => throw new RuntimeException(s"unrecognized operation: ${operation}")
        }
      }
    }

    /**
     * Entry point of this IntCode computer: execute this program with this input
     */
    def runProgram(program: Seq[Int], input: Int): OutputState = {
      @tailrec
      def loop(prog: Seq[Int], opPointer: Int): OutputState = {
        val operation = Operation(ParamMode.immediateMode(prog, opPointer))

        operation(InputState(prog, opPointer, input)) match {
          case Some(OutputState(updatedProgram, updatedPointer)) => loop(updatedProgram, updatedPointer)
          case None => OutputState(prog, opPointer)
        }
      }

      loop(program, 0)
    }
  }

}
