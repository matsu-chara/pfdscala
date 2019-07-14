package chapter02

import org.scalatest.{DiagrammedAssertions, FunSuite}

class ListImpl2Test extends FunSuite with DiagrammedAssertions {

  import ListImpl2._
  import MyList._

  test("concat") {
    val xs = Cons(1, Cons(2, Cons(3, Nil)))
    val ys = Cons(4, Cons(5, Cons(6, Nil)))

    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))

    assert(concat(xs, ys) == expected)
  }
  test("update") {
    val xs = Cons(1, Cons(2, Cons(3, Nil)))

    val expected = Cons(1, Cons(9, Cons(3, Nil)))

    assert(update(xs, 1, 9) == expected)
  }

  test("suffixes") {
    val xs = Cons(1, Cons(2, Cons(3, Nil)))

    val expected =
      Cons(
        Cons(1, Cons(2, Cons(3, Nil))),
        Cons(
          Cons(2, Cons(3, Nil)),
          Cons(
            Cons(3, Nil),
            Nil
          )
        )
      )

    assert(suffixes(xs) == expected)
  }
}
