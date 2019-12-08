package advent


import scala.annotation.tailrec

object Day4Parts extends App {

  // brute force on that one (I guess we could generate and check less numbers...)
  val countPart1 = LazyList.range(359282, 820401).count(meetsCriteriaPart1)
  println(s"Advent of code 2019 - Day 4 / part 1: number of valid passwords: ${countPart1}") // 511

  val countPart2 = LazyList.range(359282, 820401).count(meetsCriteriaPart2)
  println(s"Advent of code 2019 - Day 4 / part 2: number of valid passwords: ${countPart2}") // 316

  def meetsCriteriaPart1(password: Int): Boolean = {
    val passStr = password.toString
    Day4.containsDoubleDigit(passStr) && Day4.nonDecreasing(passStr)
  }

  def meetsCriteriaPart2(password: Int): Boolean = {
    val passStr = password.toString
    Day4.containsAtLeastOneExactDouble(passStr) && Day4.nonDecreasing(passStr)
  }

}

object Day4 {

  // determines if 2 consecutive letters are identical in this string
  def containsDoubleDigit(password: String): Boolean = {
    @tailrec
    def isOk(prevLetter: Char, rest: String): Boolean =
      prevLetter == rest.head || (rest.tail.nonEmpty && isOk(rest.head, rest.tail))

    password.length > 1 && isOk(password.head, password.tail)
  }

  // determines if digits do not decrease
  def nonDecreasing(password: String): Boolean = {
    @tailrec
    def isOk(prevDigit: Int, rest: Seq[Int]): Boolean =
      prevDigit <= rest.head && (rest.tail.isEmpty || isOk(rest.head, rest.tail))

    val digits = password.toSeq.map(digit => Integer.parseInt(digit.toString))

    digits.length > 1 && isOk(digits.head, digits.tail)
  }

  //determines if there is at least one sequence of 2 repeated digits not part of a longer repetition
  def containsAtLeastOneExactDouble(password: String): Boolean = {
    @tailrec
    def atLeastOneExactDouble(part: Seq[Char]): Boolean =
      part.nonEmpty && (
        part.takeWhile( _ == part.head).length == 2 || atLeastOneExactDouble(part.dropWhile(_ == part.head))
      )

    atLeastOneExactDouble(password.toSeq)
  }

}
