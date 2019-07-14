package chapter9.one_two

import org.scalatest.{DiagrammedAssertions, FunSuite}

class DigitTest extends FunSuite with DiagrammedAssertions {

  import Digit._

  test("testInc") {
    val check = (str: String, expect: String) => {
      assert(formatToString(inc(fromString(str))) === expect)
    }

    Seq(
      ("1", "2"), // 1, 2
      ("2", "11"), // 2, 3
      ("11", "21"), // 3, 4
      ("21", "12"), // 4, 5
      ("12", "22"), // 5, 6
      ("22", "111"), // 6, 7
    ).foreach(check.tupled)
  }

  test("testDec") {
    val check = (str: String, expect: String) => {
      assert(formatToString(dec(fromString(str))) === expect)
    }

    Seq(
      ("111", "22"), // 7, 6
      ("22", "12"), // 6, 5
      ("12", "21"), // 5, 4
      ("21", "11"), // 4, 3
      ("11", "2"), // 3, 2
      ("2", "1"), // 2, 1
    ).foreach(check.tupled)
  }

  test("testAdd") {
    val check = (str1: String, str2: String, expect: String) => {
      assert(formatToString(add(fromString(str1), fromString(str2))) === expect)
    }

    Seq(
      ("1", "1", "2"), // 1+1 = 2
      ("1", "2", "11"), // 1+2 = 3
      ("11", "2", "12"), // 3+2 = 5
      ("12", "1", "22"), // 5+1 = 6
      ("111", "22", "122"), // 7+6 = 13
    ).foreach(check.tupled)
  }
}
