package advent

import java.io.{BufferedReader, InputStreamReader}

import advent.Day13.{Ball, Coord, Paddle, Score, Tile}
import advent.Day9.IntCodeComputer.State

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.{Failure, Success, Try}


object Day13Part1 extends App {

  val program = Day9.loadProgram("src/main/data/day13-arcade-program.txt")
  val stopped = Day9.IntCodeComputer.start(program, Seq.empty)
  val parsedOutput = Day13.parsedGameOutput(stopped.output)._2

  val counted = parsedOutput.groupBy(_._2).view.mapValues(_.size).toMap

  println(s"Advent of code 2019 - Day 13 / part 1: number of block tiles: ${counted(Day13.Block)}") // 284
}


// this only prints correctly on sbt console, not
object Day13Part2 extends App {
  val paidProgram = 2L +: Day9.loadProgram("src/main/data/day13-arcade-program.txt").tail

  Stty.disableEcho()
  Stty.bufferByCharacter()
  Stty.clearScreen()

  @tailrec
  def gameTurn(game: Game, ballPredictedLanding: Option[Long]): Score = {

    Day13.printGameScreen(game.blocks, game.score)

    if (game.programState.isStopped)
      game.score

    else {

      val ballTarget = ballPredictedLanding.getOrElse(Game.predictedLandingX(game))
      Stty.printAt(1,40, s"paddle position: ${game.paddleCoord.x}, predicted ball landing: $ballTarget    ")

      // resetting the predicted target when the ball bounces back
      val nextTarget = if (game.ballCoord.y == 18) None else Some(ballTarget)

      if (game.paddleCoord.x == ballTarget) gameTurn(game.stay, nextTarget)
      else if (game.paddleCoord.x < ballTarget) gameTurn(game.goRight,nextTarget)
      else gameTurn(game.goLeft, nextTarget)

    }
  }

  val score = gameTurn(Game(Day9.IntCodeComputer.start(paidProgram, Seq.empty)), None)
  println(s"Advent of code 2019 - Day 13 / part 2: score: ${score.value}") // 13581

}

/**
 * State of the game
 */
case class Game(programState: State, blocks: Map[Coord, Tile], score: Score) {

  lazy val goLeft = goto(-1)
  lazy val goRight = goto(1)
  lazy val stay = goto(0)

  lazy val ballCoord: Coord = blocks.toList.find{case (_, tile) => tile == Ball}.map(_._1).get
  lazy val paddleCoord: Coord = blocks.toList.find{case (_, tile) => tile == Paddle}.map(_._1).get

  private def goto(direction: Int ): Game= {
    val newState = Day9.IntCodeComputer.resume(programState, direction)
    val (points, updatedBlocks) = Day13.parsedGameOutput(newState.output)
    Game(newState, blocks ++ updatedBlocks, Score(Math.max(score.value, points.value)))
  }

}
object Game {
  def apply(firstState: State): Game = {
    val (points, screen) = Day13.parsedGameOutput(firstState.output)
    new Game(firstState, screen, points)
  }

  /** finds the landing position of the ball when we simulate the continuation of this game up to the end */
  @tailrec
  def predictedLandingX(game: Game): Long =
    if (game.ballCoord.y == 18 || game.programState.isStopped) game.ballCoord.x
    else predictedLandingX(game.stay)

}

/**
 * Reader for keyboard input. Not useful finally, though handy to keep around
 */
object Keyboard {

  sealed trait Key

  case object LEFT extends Key

  case object RIGHT extends Key

  case object NEUTRAL extends Key

  case object EXIT extends Key

  object Key {
    def apply(intKey: Int) = {
      intKey match {
        case 120 => EXIT // x
        case 110 => LEFT // n
        case 105 => RIGHT // i
        case _ => NEUTRAL // anything else
      }
    }
  }

  private val buffer = new BufferedReader(new InputStreamReader(System.in))
  private var currentlyReading: Option[Future[Key]] = None

  /**
   * Listens to a keypress for max timeoutMillis, then return the key or NEUTRAL in case of timeout
   */
  def read(timeoutMillis: Long): Key = {

    val readKey = currentlyReading.getOrElse(Future {
      Key(buffer.read())
    })

    Try(Await.result(readKey, Duration(timeoutMillis, "ms"))) match {

      case Success(someKey) =>
        currentlyReading = None
        someKey

      case Failure(_: TimeoutException) =>
        currentlyReading = Some(readKey)
        NEUTRAL

    }
  }
}


object Day13 {

  sealed trait Tile {
    val letter: Char
    override def toString(): String = letter.toString
  }

  object Tile {
    def apply(code: Long): Tile = code match {
      case 0 => Empty
      case 1 => Wall
      case 2 => Block
      case 3 => Paddle
      case 4 => Ball
    }
  }

  object Empty extends Tile {
    override val letter: Char = ' '
  }

  object Wall extends Tile {
    override val letter: Char = '█'
  }

  object Block extends Tile {
    override val letter: Char = '░'
  }

  object Paddle extends Tile {
    override val letter: Char = 'T'
  }

  object Ball extends Tile {
    override val letter: Char = 'o'
  }

  case class Coord(x: Long, y: Long) {
    override def toString: String = s"$x,$y"
  }

  case class Score(value: Long) extends AnyVal

  def parsedGameOutput(programOutput: Seq[Long]): (Score, Map[Coord, Tile]) =
    programOutput
      .grouped(3)
      .foldLeft((Score(0), Map.empty[Coord, Tile])) {
        case ((_, screen), Seq(-1, 0, scoreValue)) => (Score(scoreValue), screen)
        case ((prevScore, screen), Seq(x, y, tileId)) => (prevScore, screen + (Coord(x, y) -> Tile(tileId)))
      }

  /** finds position of the pallet and the ball */
  def positions(screen: Map[Coord, Tile]): (Option[Coord], Option[Coord]) =
    (screen.toList.find{case (_, tile) => tile == Ball}.map(_._1),
      screen.toList.find{case (_, tile) => tile == Paddle}.map(_._1))

  def printGameScreen(screen: Map[Coord, Tile], score: Score): Unit = {
    screen.foreach{
      case (Coord(x, y), tile)=> Stty.printAt(x.intValue, y.intValue+1, tile.letter.toString)
    }
    Stty.printAt(50, 2, s"score: ${score.value}")
  }
}


// heavily inspired from
// https://github.com/jsuereth/streamerz/blob/master/ansi/src/main/scala/com/jsuereth/ansi/Stty.scala
object Stty {

  // TODO - this is the executable we call for STTY.  we may want to allow this to be configured or looked up.
  private val STTY = "stty"
  private val SHELL = "/bin/sh"

  /** disables echoing characters as they are typed, if this system supports STTY. */
  def disableEcho(): Unit = exec("-echo")

  /** Enables echoing characters as they are typed, if this system supports STTY. */
  def enableEcho(): Unit = exec("echo")

  def clearScreen(): Unit = print("\u001b[2J")

  /** Configures the console to buffer by character. */
  def bufferByCharacter(): Unit = exec("-icanon min 1")

  def printAt(x: Int, y: Int, line: String): Unit = print(s"\u001B[$y;${x}H$line")

  /** Executes Stty command with given argument string. */
  def exec(args: String): Unit = {
    val cmd = s"$STTY $args < /dev/tty"
    val p = Runtime.getRuntime.exec(Array(s"$SHELL", "-c", cmd))
    p.waitFor() match {
      case 0 => ()
      case n =>
        // TODO - Figure out what to do here...
        throw new UnsupportedOperationException(s"It appears your environment does not support STTY, tried to call [$cmd], return value $n")
    }
  }
}
