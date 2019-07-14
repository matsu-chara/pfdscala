package chapter11.catenable.part1

import cats.Eval
import chapter08.bankers_deque.BankersDeque

sealed trait SimpleCatenableDeque[+A] {

  import SimpleCatenableDeque._

  def isEmpty: Boolean = this match {
    case Shallow(x) => x.isEmpty
    case _ => false
  }

  def cons[B >: A](x: B): SimpleCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.cons(x))
    case Deep(f, m, r) => Deep(f.cons(x), m, r)
  }

  def ++[B >: A](cat2: SimpleCatenableDeque[B]): SimpleCatenableDeque[B] = (this, cat2) match {
    case (Shallow(d1), Shallow(d2)) =>
      if (tooSmall(d1)) Shallow(dappendL(d1, d2))
      else if (tooSmall(d2)) Shallow(dappendR(d1, d2))
      else Deep(d1, Eval.now(SimpleCatenableDeque.empty), d2)
    case (Shallow(d), Deep(f, m, r)) =>
      if (tooSmall(d)) Deep(dappendL(d, f), m, r)
      else Deep(d, Eval.later(m.value.cons(f)), r)
    case (Deep(f, m, r), Shallow(d)) =>
      if (tooSmall(d)) Deep(f, m, dappendR(r, d))
      else Deep(f, Eval.later(m.value.snoc(r)), d)
    case (Deep(f1, m1, r1), Deep(f2, m2, r2)) =>
      Deep(f1, Eval.later(m1.value.snoc(r1) ++ m2.value.cons(f2)), r2)
  }

  def snoc[B >: A](x: B): SimpleCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.snoc(x))
    case Deep(f, m, r) => Deep(f, m, r.snoc(x))
  }

  def head: A = this match {
    case Shallow(d) => d.head
    case Deep(f, _, _) => f.head
  }

  def tail: SimpleCatenableDeque[A] = this match {
    case Shallow(d) => Shallow(d.tail)
    case Deep(f, m, r) =>
      val f_ = f.tail
      if (!tooSmall(f_)) Deep(f_, m, r)
      else if (m.value.isEmpty) Shallow(dappendL(f_, r))
      else Deep(dappendL(f_, m.value.head), Eval.later(m.value.tail), r)
  }

  def last: A = this match {
    case Shallow(d) => d.last
    case Deep(_, _, r) => r.last
  }

  def init: SimpleCatenableDeque[A] = this match {
    case Shallow(d) => Shallow(d.init)
    case Deep(f, m, r) =>
      val r_ = r.init
      if (!tooSmall(r_)) Deep(f, m, r_)
      else if (m.value.isEmpty) Shallow(dappendR(f, r_))
      else Deep(f, Eval.later(m.value.init), dappendR(m.value.last, r_))
  }

}

object SimpleCatenableDeque {
  val empty: SimpleCatenableDeque[Nothing] = Shallow(BankersDeque.empty)

  private def tooSmall[A](x: BankersDeque[A]): Boolean = {
    x.isEmpty || x.tail.isEmpty
  }

  private def dappendL[A](d1: BankersDeque[A], d2: BankersDeque[A]): BankersDeque[A] = {
    if (d1.isEmpty) d2 else d2.cons(d1.head)
  }

  private def dappendR[A](d1: BankersDeque[A], d2: BankersDeque[A]): BankersDeque[A] = {
    if (d2.isEmpty) d1 else d1.snoc(d2.head)
  }

  case class Shallow[A](x: BankersDeque[A]) extends SimpleCatenableDeque[A]
  case class Deep[A](f: BankersDeque[A], m: Eval[SimpleCatenableDeque[BankersDeque[A]]], r: BankersDeque[A])
    extends SimpleCatenableDeque[A]

}
