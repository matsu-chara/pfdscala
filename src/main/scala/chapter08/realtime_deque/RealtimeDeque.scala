package chapter08.realtime_deque

case class RealtimeDeque[A](lenf: Int, f: LazyList[A], sf: LazyList[A], lenr: Int, r: LazyList[A], sr: LazyList[A]) {

  import RealtimeDeque._

  def isEmpty: Boolean = {
    lenf + lenr == 0
  }

  def snoc(a: A): RealtimeDeque[A] = reverse.cons(a).reverse

  def cons(a: A): RealtimeDeque[A] = {
    check(lenf + 1, a #:: f, exec1(sf), lenr, r, exec1(sr))
  }

  def reverse: RealtimeDeque[A] = RealtimeDeque(lenr, r, sr, lenf, f, sf)

  def last: A = reverse.head

  def head: A = (f, r) match {
    case (LNil, LNil) => throw new NoSuchElementException
    case (LNil, x #:: _) => x
    case (x #:: _, _) => x
  }

  def init: RealtimeDeque[A] = reverse.tail.reverse

  def tail: RealtimeDeque[A] = (f, r) match {
    case (LNil, LNil) => throw new NoSuchElementException
    case (LNil, _ #:: r_) => empty
    case (_ #:: f_, r_) => check(lenf - 1, f_, exec2(sf), lenr, r_, exec2(sr))
  }
}

object RealtimeDeque {
  private val c = 2

  private val LNil = LazyList.empty[Nothing]

  def empty[A]: RealtimeDeque[A] =
    RealtimeDeque[A](0, LazyList.empty, LazyList.empty, 0, LazyList.empty, LazyList.empty)

  def check[A](lenf: Int,
               f: LazyList[A],
               sf: LazyList[A],
               lenr: Int,
               r: LazyList[A],
               sr: LazyList[A]): RealtimeDeque[A] = {
    if (lenf > c * lenr + 1) {
      val i = (lenf + lenr) / 2
      val j = lenf + lenr - 1
      val f_ = f.take(i)
      val r_ = rotateDrop(r, i, f)
      RealtimeDeque(i, f_, f, j, r_, r_)
    } else if (lenr > c * lenf + 1) {
      val j = (lenf + lenr) / 2
      val i = lenf + lenr - 1
      val r_ = r.take(j)
      val f_ = rotateDrop(f, j, r)
      RealtimeDeque(i, f_, f_, j, r_, r_)
    } else {
      RealtimeDeque(lenf, f, sf, lenr, r, sr)
    }
  }

  private def exec2[A](xs: LazyList[A]): LazyList[A] = exec1(exec1(xs))

  private def exec1[A](xs: LazyList[A]): LazyList[A] = xs match {
    case _ #:: s => s
    case s => s
  }

  private def rotateRev[A](f: LazyList[A], r: LazyList[A], a: LazyList[A]): LazyList[A] = (f, r, a) match {
    case (LNil, r_, a_) => r_.reverse ++ a_
    case (x #:: f_, r_, a_) => x #:: rotateRev(f_, r_.drop(c), r_.take(c).reverse ++ a_)
  }

  private def rotateDrop[A](f: LazyList[A], j: Int, r: LazyList[A]): LazyList[A] = {
    if (j < c) {
      rotateRev(f, r.drop(j), LNil)
    } else {
      f.head #:: rotateDrop(f.tail, j - c, r.drop(c))
    }
  }
}
