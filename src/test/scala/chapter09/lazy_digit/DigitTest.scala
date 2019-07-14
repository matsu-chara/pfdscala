package chapter09.lazy_digit

import chapter09.lazy_digit.Digit._
import org.scalatest.{DiagrammedAssertions, FunSuite}

class DigitTest extends FunSuite with DiagrammedAssertions {

  test("testInc") {
    val check = (str: String, expect: String) => {
      assert(formatToString(inc(fromString(str))) === expect)
    }

    Seq(
      ("222222", "1111111"),
    ).foreach(check.tupled)
  }
  test("testDec") {
    val check = (str: String, expect: String) => {
      assert(formatToString(dec(fromString(str))) === expect)
    }

    Seq(
      ("1111111", "0111111"),
    ).foreach(check.tupled)
  }

  test("testInc/Dec") {
    val check = (str: String, expect: String) => {
      assert(formatToString(dec(inc(fromString(str)))) === expect)
    }

    Seq(
      ("222222", "0111111"), // same meaning, but different format
    ).foreach(check.tupled)
  }

}
