package advent

import org.scalatest.{FlatSpec, Matchers}

class Day4Spec extends FlatSpec with Matchers {

  "containsDoubleDigit" should "detect repetitions" in {
    Day4.containsDoubleDigit("11") should be (true)
    Day4.containsDoubleDigit("133") should be (true)
    Day4.containsDoubleDigit("881") should be (true)
    Day4.containsDoubleDigit("1234557") should be (true)
  }

  "containsDoubleDigit" should "detect non repetitions" in {
    Day4.containsDoubleDigit("") should be(false)
    Day4.containsDoubleDigit("9") should be(false)
    Day4.containsDoubleDigit("1234567") should be(false)
    Day4.containsDoubleDigit("654321") should be(false)
    Day4.containsDoubleDigit("12121212") should be(false)
  }

  "nonDecreasing" should "work as expected" in {
    Day4.nonDecreasing("111111") should be (true)
    Day4.nonDecreasing("223450") should be (false)
    Day4.nonDecreasing("123789") should be (true)
  }

  "exactlyOneDouble" should "accept only repetition of exactly 2" in {
    Day4.containsAtLeastOneExactDouble("112233") should be (true)
    Day4.containsAtLeastOneExactDouble("123444") should be (false)
    Day4.containsAtLeastOneExactDouble("111122") should be (true)
    Day4.containsAtLeastOneExactDouble("111222") should be (false)
    Day4.containsAtLeastOneExactDouble("112224") should be (true)
    Day4.containsAtLeastOneExactDouble("222224") should be (false)
    Day4.containsAtLeastOneExactDouble("222244") should be (true)
  }

}
