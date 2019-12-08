package advent

import advent.Day4.{containsDoubleDigit, nonDecreasing}

import scala.annotation.tailrec

object Day4Part1 extends App {

  // brute force on that one (I guess we could generate and check less numbers...)
  val countPart1 = LazyList.range(359282, 820401).count(meetsCriteria)
  println(s"Advent of code 2019 - Day 4 / part 1: number of valid passwords: ${countPart1}") // 511

  def meetsCriteria(password: Int): Boolean = {
    val passStr = password.toString
    containsDoubleDigit(passStr) && nonDecreasing(passStr)
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

}
