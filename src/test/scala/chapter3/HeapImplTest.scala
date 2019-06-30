package chapter3

import org.scalatest.{DiagrammedAssertions, FunSuite}

class HeapImplTest extends FunSuite with DiagrammedAssertions {

  test("testMerge") {
    import Heap._
    import HeapImpl._

    val empty = Empty[Int]()
    val i1 = insert(1, empty)
    val i2 = insert(2, i1)
    val i3 = insert(3, i2)
    val i4 = insert(1004, i2)
    val i5 = insert(5, i2)

    val j1 = insert2(1, empty)
    val j2 = insert2(2, i1)
    val j3 = insert2(3, i2)
    val j4 = insert2(1004, i2)
    val j5 = insert2(5, i2)

    assert(i5 == j5)
  }

}
