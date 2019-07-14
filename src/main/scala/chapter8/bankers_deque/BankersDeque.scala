package chapter8.bankers_deque

trait Deque[+A] {
  def isEmpty: Boolean

  def cons[B >: A](a: B): Deque[B]

  def head: A

  def tail: Deque[A]

  def snoc[B >: A](a: B): Deque[B]

  def last: A

  def init: Deque[A]
}

case class BankersDeque[+A](lenf: Int, f: LazyList[A], lenr: Int, r: LazyList[A]) extends Deque[A] {

  import BankersDeque._

  override def isEmpty: Boolean = {
    lenf + lenr == 0
  }

  override def snoc[B >: A](a: B): BankersDeque[B] = reverse.cons(a).reverse

  override def cons[B >: A](a: B): BankersDeque[B] = {
    check(lenf + 1, a #:: f, lenr, r)
  }

  override def last: A = reverse.head

  override def head: A = (f, r) match {
    case (LNil, x #:: _) => x
    case (x #:: _, _) => x
  }

  def reverse: BankersDeque[A] = BankersDeque(lenr, r, lenf, f)

  override def init: BankersDeque[A] = reverse.tail.reverse

  override def tail: BankersDeque[A] = (f, r) match {
    case (LNil, _ #:: r_) => empty
    case (_ #:: f_, r_) => check(lenf - 1, f_, lenr, r)
  }
}

object BankersDeque {
  private val c = 1

  private val LNil = LazyList.empty[Nothing]

  def empty[A]: BankersDeque[A] = BankersDeque[A](0, LazyList.empty, 0, LazyList.empty)

  def check[A](lenf: Int, f: LazyList[A], lenr: Int, r: LazyList[A]): BankersDeque[A] = {
    if (lenf > c * lenr + 1) {
      val i = (lenf + lenr) / 2
      val j = lenf + lenr - 1
      val f_ = f.take(i)
      val r_ = r ++ f.drop(i).reverse
      BankersDeque(i, f_, j, r_)
    } else if (lenr > c * lenf + 1) {
      val j = (lenf + lenr) / 2
      val i = lenf + lenr - 1
      val r_ = r.take(j)
      val f_ = f ++ r.drop(j).reverse
      BankersDeque(i, f_, j, r_)
    } else {
      BankersDeque(lenf, f, lenr, r)
    }
  }
}
