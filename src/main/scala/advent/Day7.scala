package advent

import advent.Day7.IntCodeComputer.State

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

/**
 * Solution to day7 part 1
 *
 * In this settings, we assume that the program never pauses => all inputs are always available at start.
 * We also assumes that each execution is only returning 1 parameter
 *
 */
object Day7Part1 extends App {

  val (phaseSettings, power) = optimalPhases(Day7.day7Program)
  println(s"Advent of code 2019 - Day 7 / part 1: $power with settings $phaseSettings")

  // runs that program as many times as there are phase settings, chaining each output to as input for the next amp
  def chainAmplifiers(amplifierControlSoftware: Seq[Int], phaseSettings: Seq[Int]): Int = {
    val intCodeComputer = new Day7.IntCodeComputer(amplifierControlSoftware)
    phaseSettings.foldLeft(0) { (prevOutput, phase) => intCodeComputer.start(Seq(phase, prevOutput)).output.head }
  }

  // finds the set of phase settings that yield the best thrust power
  def optimalPhases(amplifierControlSoftware: Seq[Int]): (Seq[Int], Int) = {
    List(0, 1, 2, 3, 4)
      .permutations
      .map(phaseSettings => (phaseSettings, chainAmplifiers(amplifierControlSoftware, phaseSettings)))
      .reduce((run1, run2) => if (run1._2 > run2._2) run1 else run2)
  }
}


/**
 * Solution to day7 part 2
 *
 * in this setting, we assume that programs are pausing when not enough input is available
 */
object Day7Part2 extends App {

  val (phaseSettings, power) = optimalPhases(Day7.day7Program)
  println(s"Advent of code 2019 - Day 7 / part 2: $power with settings $phaseSettings")

  // runs the amplifiers in loop, chain them into each other then repeating until one of them is stopped
  def loopingAmplifiersChain(amplifierControlSoftware: Seq[Int], phaseSettings: Seq[Int]): Int = {
    val intCodeComputer = new Day7.IntCodeComputer(amplifierControlSoftware)

    val Seq(phaseA, phaseB, phaseC, phaseD, phaseE) = phaseSettings

    // initial execution of each program, using the phases as input
    val pausedStateA = intCodeComputer.start(Seq(phaseA, 0), "A")
    val pausedStateB = intCodeComputer.start(phaseB +: pausedStateA.output, "B")
    val pausedStateC = intCodeComputer.start(phaseC +: pausedStateB.output, "C")
    val pausedStateD = intCodeComputer.start(phaseD +: pausedStateC.output, "D")
    val pausedStateE = intCodeComputer.start(phaseE +: pausedStateD.output, "E")

    @tailrec
    def runAmp(ampStates: Seq[State]): Int = {

      if (ampStates.exists(_.isStopped))
        ampStates.last.output.head  // this assumes a stopped program always returns one single output
      else {
        val executedA = intCodeComputer.resume(ampStates.head, ampStates.last.output)
        val newStates = ampStates.tail.foldLeft(Seq(executedA)) {
          (executedStates, state) => {intCodeComputer.resume(state, executedStates.head.output) +: executedStates}
        }
        runAmp(newStates.reverse)
      }
    }

    runAmp(Seq(pausedStateA, pausedStateB, pausedStateC, pausedStateD, pausedStateE))
  }

  // finds the set of phase settings that yield the best thrust power
  def optimalPhases(amplifierControlSoftware: Seq[Int]): (Seq[Int], Int) = {
    List(5,6,7,8,9)
      .permutations
      .map(phaseSettings => (phaseSettings, loopingAmplifiersChain(amplifierControlSoftware, phaseSettings)))
      .reduce((run1, run2) => if (run1._2 > run2._2) run1 else run2)
  }

}


object Day7 {

  val day7Program =
    Using(Source.fromFile("src/main/data/day7-amplifier-program.txt")) { file => file.getLines().toSeq.head }
      .get
      .split(",").map(Integer.parseInt)
      .toSeq

  /**
   * 3rd iteration on the intCode computer:
   *
   * This one accepts a sequence of inputs and provides a Seq of outputs.
   *
   * When an input is read by the program, it is un-piled from the stack and no longer available.
   *
   * If no input is available during a read operation, the program pause, and can be resumed later,
   *
   */
  class IntCodeComputer(startProgram: Seq[Int]) {

    /**
     * Main entry point of this IntCode computer: execute this program with this input and provides the resulting state
     * as return value
     */
    def start(inputs: Seq[Int], label: String = "noname"): State = loop(new State(startProgram, inputs, label))

    /**
     * Resumes the execution of this program, keeping the paused programme and pointer but resetting its i/o
     */
    def resume(state: State, input: Seq[Int]): State = {
      assert(state.isPaused, "inconsistent state: cannot resume a non paused program")
      loop(state.unpause(input))
    }

    // convenience executor, when there is only one input (backward compatibility with old tests)
    def start(input: Int): State = start(Seq(input))

    @tailrec private def loop(state: State): State = if (state.isRunning) {
      println(s"program: ${state.program}, pointer ${state.opPointer}, output ${state.output}")
      loop(state.step)
    } else state
  }

  object IntCodeComputer {

    case class State(program: Seq[Int], opPointer: Int, label: String,
                     input: Seq[Int], output: Seq[Int], isStopped: Boolean, isPaused: Boolean) {

      def this(program: Seq[Int], input: Seq[Int], label: String = "noname") = this(program, 0, label, input, Nil, false, false)

      // move the operation pointer to the specified location
      def jumpTo(newOpPointer: Int): State = copy(opPointer = newOpPointer)

      // moves the operation pointer forward
      def forward(size: Int): State = jumpTo(opPointer + size)

      // adds this to the program output
      def addOutput(moreOutPut: Int): State = copy(output = moreOutPut +: output)

      lazy val stopped = copy(isStopped = true, output = output.reverse)
      lazy val paused = copy(isPaused = true)
      lazy val isRunning = !isStopped && !isPaused

      // un-pause this program, using those new inputs
      def unpause(input: Seq[Int]): State = copy(isPaused = false, input = input, output = Nil)

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

      // tries to pop one input, if available
      lazy val readInput: Option[(Int, State)] =
        input.headOption.map(existingInput => (existingInput, copy(input = input.tail)))
    }

    // accessor of a function parameter in the program
    type Param = State => Int

    object Param {

      // param order: position among the parameters of the function this parameter belongs to, starting at 0
      def immediateMode(paramPosition: Int): Param = state => state.program(state.opPointer + paramPosition + 1)

      def positionMode(paramPosition: Int): Param =
        state => state.program(state.program(state.opPointer + paramPosition + 1))

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

      //  looks up 2 values and writes their sum at the specified location
      def addition(val1: Param, val2: Param): Operation =
        state => state.write(2, val1(state) + val2(state)).forward(4)

      // looks up 2 values and writes their product at the specified location
      def multiplication(val1: Param, val2: Param): Operation =
        state => state.write(2, val1(state) * val2(state)).forward(4)

      // consumes one input and writes it in the program at the specified location, or pause if no input available
      val input: Operation = (state: State) =>
        state.readInput match {
          case Some((consumedInput, updatedState)) =>
            updatedState.write(0, consumedInput).forward(2)
          case None => state.paused
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

      // determines the Operation corresponding to that intCode
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
