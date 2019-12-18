package advent


import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day9Part1 extends App {
  val boostCode = Day9.IntCodeComputer.start(Day9.day9Program, 1).output
  println(s"Advent of code 2019 - Day 9 / part 1: BOOST code: $boostCode")  // 3839402290
}

object Day9Part2 extends App {
  val result = Day9.IntCodeComputer.start(Day9.day9Program, 2).output
  println(s"Advent of code 2019 - Day 9 / part 2:: $result")  // 35734
}

object Day9 {

  lazy val day9Program = loadProgram("src/main/data/day9-program.txt")

  def loadProgram(filePath: String): Seq[Long] = Using(Source.fromFile(filePath)) { file => file.getLines().toSeq.head }
    .get
    .split(",").map(_.toLong)
    .toSeq

  /**
   * 4th iteration on the intCode computer: with relative memory access when writing and large memory
   */
  object IntCodeComputer {

    def start(startProgram: Seq[Long], inputs: Seq[Long], label: String = "noname"): State = {
      val memory = Seq.fill(10000)(0L)
      loop(new State(startProgram ++: memory, inputs, label))
    }

    def resume(state: State, input: Int): State = resume(state, Seq(input.longValue()))

    def resume(state: State, inputs: Seq[Long]): State = {
      assert(state.isPaused, "inconsistent state: cannot resume a non paused program")
      loop(state.unpause(inputs))
    }

    // (convenience starter, when there is only one input, for backward compatibility with old tests))
    def start(startProgram: Seq[Long], input: Int): State = start(startProgram, Seq(input.toLong))

    @tailrec private def loop(state: State): State =
      if (state.isRunning) {
        //        println(s"program: ${state.program.take(15)}, pointer ${state.opPointer}, output ${state.output}")
        loop(Operation(state.currentOpCode)(state))
      }
      else state


    case class State(program: Seq[Long], opPointer: Int, label: String, relativeBase: Int,
                     input: Seq[Long], output: Seq[Long],
                     isStopped: Boolean, isPaused: Boolean) {
      def this(program: Seq[Long], input: Seq[Long], label: String = "noname") =
        this(program, 0, label, 0, input, Nil, false, false)

      def jumpTo(newOpPointer: Int): State = copy(opPointer = newOpPointer)

      def forward(size: Int): State = jumpTo(opPointer + size)

      def addOutput(moreOutPut: Long): State = copy(output = output :+ moreOutPut)

      lazy val stopped = copy(isStopped = true) // maybe I should truncate the additional memory here?

      lazy val paused = copy(isPaused = true)
      lazy val isRunning = !isStopped && !isPaused

      def unpause(newInputs: Seq[Long]): State = copy(isPaused = false, input = newInputs, output = Nil)

      lazy val currentOpCode: Int = program(opPointer).toInt

      lazy val readInput: Option[(Long, State)] =
        input.headOption.map(existingInput => (existingInput, copy(input = input.tail)))

      def read(p: Pointer): Long = program(p(this))

      def write(p: Pointer, value: Long): State = {
        val writePointer = p(this)
        this.copy(program = program.take(writePointer) :+ value :++ program.drop(writePointer + 1))
      }

      def addToRelativeBase(delta: Int): State = copy(relativeBase = relativeBase + delta)
    }

    type Operation = State => State

    object Operation {

      def addition(in1: Pointer, in2: Pointer, out: Pointer): Operation =
        state => state.write(out, state.read(in1) + state.read(in2)).forward(4)

      def multiplication(in1: Pointer, in2: Pointer, out: Pointer): Operation =
        state => state.write(out, state.read(in1) * state.read(in2)).forward(4)

      def input(out: Pointer): Operation = state =>
        state.readInput match {
          case Some((consumedInput, updatedState)) => updatedState.write(out, consumedInput).forward(2)
          case None => state.paused
        }

      def output(in1: Pointer): Operation = state => state.addOutput(state.read(in1)).forward(2)

      def jumpIfTrue(in1: Pointer, in2: Pointer): Operation =
        state => if (state.read(in1) != 0) state.jumpTo(state.read(in2).toInt) else state.forward(3)

      def jumpIfFalse(in1: Pointer, in2: Pointer): Operation =
        state => if (state.read(in1) == 0) state.jumpTo(state.read(in2).toInt) else state.forward(3)

      def lessThan(in1: Pointer, in2: Pointer, out: Pointer): Operation =
        state => state.write(out, if (state.read(in1) < state.read(in2)) 1 else 0).forward(4)

      def areParamsEqual(in1: Pointer, in2: Pointer, out: Pointer): Operation =
        state => state.write(out, if (state.read(in1) == state.read(in2)) 1 else 0).forward(4)

      def adjustRelativeBase(in1: Pointer): Operation = state =>
        state.addToRelativeBase(state.read(in1).toInt).forward(2)

      val exit: Operation = (state: State) => state.stopped

      // determines the Operation corresponding to that intCode
      def apply(operation: Long): Operation = {

        val operationStringRev = operation.toString.reverse

        val paramModes = operationStringRev.drop(2).padTo(3, '0')
          .zipWithIndex
          .map { case (mode, paramPosition) => Pointer(mode, paramPosition) }

        operationStringRev.take(2).replace("0", "") match {
          case "1" => addition(paramModes(0), paramModes(1), paramModes(2))
          case "2" => multiplication(paramModes(0), paramModes(1), paramModes(2))
          case "3" => input(paramModes(0))
          case "4" => output(paramModes(0))
          case "5" => jumpIfTrue(paramModes(0), paramModes(1))
          case "6" => jumpIfFalse(paramModes(0), paramModes(1))
          case "7" => lessThan(paramModes(0), paramModes(1), paramModes(2))
          case "8" => areParamsEqual(paramModes(0), paramModes(1), paramModes(2))
          case "9" => adjustRelativeBase(paramModes(0))
          case "99" => exit
          case _ => throw new RuntimeException(s"unrecognized operation: ${operation}")
        }
      }
    }

    type Pointer = State => Int

    object Pointer {

      def immediateMode(paramPosition: Int): Pointer = state => state.opPointer + paramPosition + 1

      def positionMode(paramPosition: Int): Pointer = state => state.program(state.opPointer + paramPosition + 1).toInt

      def relativeMode(paramPosition: Int): Pointer = state =>
        state.program(state.opPointer + paramPosition + 1).toInt + state.relativeBase

      def apply(encodedMode: Char, paramPosition: Int): Pointer =
        encodedMode match {
          case '0' => positionMode(paramPosition)
          case '1' => immediateMode(paramPosition)
          case '2' => relativeMode(paramPosition)
          case _ => throw new RuntimeException(s"unknown parameter mode: $encodedMode")
        }
    }

  }

}
