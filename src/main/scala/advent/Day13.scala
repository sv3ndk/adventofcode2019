package advent

object Day13 extends App {

  val program = Day9.loadProgram("src/main/data/day13-arcade-program.txt")
  val stopped = Day9.IntCodeComputer.start(program, Seq.empty)

  val parsedOutput = stopped.output
    .grouped(3)
    .foldLeft(Seq.empty[(Long, Long, Long)]) {
      case (parsed, Seq(x, y, tileId)) => (x, y, tileId) +: parsed
    }

  val counted = parsedOutput.groupBy(_._3).view.mapValues(_.size).toMap

  val BLOCK_TILE = 2
  println(s"Advent of code 2019 - Day 13 / part 1: number of block tiles: ${counted(BLOCK_TILE)}") // 284

}
