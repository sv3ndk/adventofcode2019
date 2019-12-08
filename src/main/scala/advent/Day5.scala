package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day5Part1 extends App {
  Using(Source.fromFile("src/main/data/day5-program.txt")) {
    file => Day5.IntCodeComputer.runProgram(
      Day5.parseProgram(file.getLines().next()),
      1)    // 8332629
  }
}

object Day5Part2 extends App {
  Using(Source.fromFile("src/main/data/day5-program.txt")) {
    file => Day5.IntCodeComputer.runProgram(
      Day5.parseProgram(file.getLines().next()),
      5)  // 8805067
  }
}


object Day5 {

  def parseProgram(line: String) = line.split(",").map(Integer.parseInt)

  object IntCodeComputer {

    // accessor of a parameter in the program
    type ParamMode = (Seq[Int], Int) => Int

    object ParamMode {

      // param order position in the parameter orderof that function, starting at 0
      def immediateMode(paramPosition: Int): ParamMode = (program, pointer) => program(pointer + paramPosition + 1)
      def positionMode(paramPosition: Int): ParamMode = (program, pointer) => program(program(pointer+ paramPosition + 1))

      def apply(encodedMode: Char, paramPosition: Int): ParamMode =
        encodedMode match {
          case '0' => positionMode(paramPosition)
          case '1' => immediateMode(paramPosition)
          case _ => throw new RuntimeException(s"unknown parameter mode: $encodedMode")
        }
    }

    case class InputState(program: Seq[Int], opPointer: Int, input: Int) {
      // no operation => just jumps the pointer to the next operation
      def noop(paramNumber: Int): OutputState = OutputState(program, opPointer + paramNumber + 1)
      def jumpTo(newOpPointer: Int) : OutputState = OutputState(program, newOpPointer)
    }

    case class OutputState(program: Seq[Int], opPointer: Int)

    // updates the program by writing that value at that location
    // writeParamPosition determines the input param containing the address where to write to
    // (cf "Parameters that an instruction writes to will never be in immediate mode.")
    def write(program: Seq[Int], opPointer: Int, writeParamPosition: Int, value: Int): Seq[Int] = {
      val writePointer = ParamMode.immediateMode(writeParamPosition)(program, opPointer)
      program.take(writePointer) :+ value :++ program.drop(writePointer + 1)
    }

    // step from one program state to another, having access to program input optionally able to specify the output
    type Operation = InputState => Option[OutputState]

    object Operation {

      // looks up 2 values (by ref or value) and writes their sum at the specified location (always in position mode)
      case class Addition(term1: ParamMode, term2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          val sum = term1(state.program, state.opPointer) + term2(state.program, state.opPointer)
          Some(OutputState(write(state.program, state.opPointer, 2, sum), state.opPointer + 4))
        }
      }

      // looks up 2 values (by ref or value) and writes their product at the specified location (always in position mode)
      case class Multiplication(factor1: ParamMode, factor2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          val product = factor1(state.program, state.opPointer) * factor2(state.program, state.opPointer)
          Some(OutputState(write(state.program, state.opPointer, 2, product), state.opPointer + 4))
        }
      }

      // writes the program input  at the specified location (always in position mode)
      val input: Operation = {
        case (InputState(program, opPointer, input)) =>
          Some(OutputState(write(program, opPointer, 0, input), opPointer + 2))
      }

      // looks up a value (by ref or value) and writes it in output
      case class Output(outputParam: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          println(s"[OUTPUT]: ${outputParam(state.program, state.opPointer)}")
          Some(state.jumpTo(state.opPointer + 2))
        }
      }

      // if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case class JumpIfTrue(param1: ParamMode, param2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          if (param1(state.program, state.opPointer) > 0)
            Some(state.jumpTo(param2(state.program, state.opPointer)))
          else
            Some(state.noop(2))
        }
      }

      // if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case class JumpIfFalse(param1: ParamMode, param2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {
          if (param1(state.program, state.opPointer) == 0)
            Some(state.jumpTo(param2(state.program, state.opPointer)))
          else
            Some(state.noop(2))
        }
      }

      // if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case class LessThan(param1: ParamMode, param2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {

          val writtenValue =
            if (param1(state.program, state.opPointer) <  param2(state.program, state.opPointer)) 1
            else 0

          Some(OutputState(write(state.program, state.opPointer,2, writtenValue), state.opPointer + 4))
        }
      }

      // if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case class Equals(param1: ParamMode, param2: ParamMode) extends Operation {
        def apply(state: InputState): Option[OutputState] = {

          val writtenValue =
            if (param1(state.program, state.opPointer) == param2(state.program, state.opPointer)) 1
            else 0

          Some(OutputState(write(state.program, state.opPointer, 2, writtenValue), state.opPointer + 4))
        }
      }


      val exit: Operation = { _ => None }

      // parses a string into the corresponding operation
      def apply(operation: Int): Operation = {
        val operationString = operation.toString.reverse

        // param modes know how to read a param: immediate mode vs position mode + where the parameter is in the stack
        val paramModes = operationString.drop(2).padTo(3, '0')
          .zipWithIndex
          .map{ case(mode, paramPosition) => ParamMode.apply(mode, paramPosition)}

      operationString.take(2).replace("0", "") match {
          case "1" => Addition(paramModes(0), paramModes(1))
          case "2" => Multiplication(paramModes(0), paramModes(1))
          case "3" => input
          case "4" => Output(paramModes(0))
          case "5" => JumpIfTrue(paramModes(0), paramModes(1))
          case "6" => JumpIfFalse(paramModes(0), paramModes(1))
          case "7" => LessThan(paramModes(0), paramModes(1))
          case "8" => Equals(paramModes(0), paramModes(1))
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
        val operation = Operation(ParamMode.immediateMode(-1)(prog, opPointer))

        operation(InputState(prog, opPointer, input)) match {
          case Some(OutputState(updatedProgram, updatedPointer)) => loop(updatedProgram, updatedPointer)
          case None => OutputState(prog, opPointer)
        }
      }

      loop(program, 0)
    }
  }

}
