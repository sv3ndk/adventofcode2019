package advent

import org.scalatest.{FlatSpec, Matchers}

class Day4Test extends FlatSpec with Matchers {

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

}
