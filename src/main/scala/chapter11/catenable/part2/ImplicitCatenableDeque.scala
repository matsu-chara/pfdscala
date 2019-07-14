package chapter11.catenable.part2

import cats.Eval
import chapter08.bankers_deque.BankersDeque

sealed trait ImplicitCatenableDeque[+A] {

  import CmpdElem._
  import ImplicitCatenableDeque._

  def isEmpty: Boolean = this match {
    case Shallow(x) => x.isEmpty
    case _ => false
  }

  def ++[B >: A](cat2: ImplicitCatenableDeque[B]): ImplicitCatenableDeque[B] = (this, cat2) match {
    case (Shallow(d1), Shallow(d2)) =>
      if (d1.size < 4) Shallow(dappendL(d1, d2))
      else if (d2.size < 4) Shallow(dappendR(d1, d2))
      else {
        val (f, m, r) = share(d1, d2)
        Deep(d1, Eval.now(empty), m, Eval.now(empty), r)
      }
    case (Shallow(d), Deep(f, a, m, b, r)) =>
      if (d.size < 4) Deep(dappendL(d, f), a, m, b, r)
      else Deep(d, Eval.later(a.value.cons(Simple(f))), m, b, r)
    case (Deep(f, a, m, b, r), Shallow(d)) =>
      if (d.size < 4) Deep(f, a, m, b, dappendR(r, d))
      else Deep(f, a, m, Eval.later(b.value.snoc(Simple(r))), d)
    case (Deep(f1, a1, m1, b1, r1), Deep(f2, a2, m2, b2, r2)) =>
      val (r1_, m, f2_) = share(r1, f2)
      val a1_ = Eval.later(a1.value.snoc(Cmpd(m1, b1, r1_)))
      val b2_ = Eval.later(b2.value.cons(Cmpd(f2_, a2, m2)))
      Deep(f1, a1_, m, b2_, r2)
  }

  def cons[B >: A](x: B): ImplicitCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.cons(x))
    case Deep(f, a, m, b, r) => Deep(f.cons(x), a, m, b, r)
  }

  def snoc[B >: A](x: B): ImplicitCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.snoc(x))
    case Deep(f, a, m, b, r) => Deep(f, a, m, b, r.snoc(x))
  }

  def head: A = this match {
    case Shallow(d) => d.head
    case Deep(f, _, _, _, _) => f.head
  }

  def last: A = this match {
    case Shallow(d) => d.last
    case Deep(_, _, _, _, r) => r.last
  }

  def tail: ImplicitCatenableDeque[A] = this match {
    case Shallow(d) => Shallow(d.tail)
    case Deep(f, a, m, b, r) =>
      if (f.size > 3) {
        Deep(f.tail, a, m, b, r)
      } else if (!a.value.isEmpty) {
        a.value.head match {
          case Simple(d) =>
            val f_ = dappendL(f.tail, d)
            Deep(f_, Eval.later(a.value.tail), m, b, r)
          case Cmpd(f_, c_, r_) =>
            val f__ = dappendL(f.tail, f_)
            val a__ = Eval.later(c_.value ++ a.value.repalceHead(Simple(r_)))
            Deep(f__, a__, m, b, r)
        }
      } else if (!b.value.isEmpty) {
        b.value.head match {
          case Simple(d) =>
            val f_ = dappendL(f.tail, m)
            Deep(f_, Eval.now(empty), d, Eval.later(b.value.tail), r)
          case Cmpd(f_, c_, r_) =>
            val f__ = dappendL(f.tail, m)
            val a__ = Eval.later(c_.value.cons(Simple(f_)))
            Deep(f__, a__, r_, Eval.later(b.value.tail), r)
        }
      } else {
        Shallow(dappendL(f.tail, m)) ++ Shallow(r)
      }
  }

  def init: ImplicitCatenableDeque[A] = this match {
    case Shallow(d) => Shallow(d.init)
    case Deep(f, a, m, b, r) =>
      if (r.size > 3) {
        Deep(f, a, m, b, r.init)
      } else if (!b.value.isEmpty) {
        b.value.last match {
          case Simple(d) =>
            val r_ = dappendR(d, r.init)
            Deep(f, a, m, Eval.later(b.value.init), r_)
          case Cmpd(f_, c_, r_) =>
            val r__ = dappendR(r_, r.init)
            val b__ = Eval.later(c_.value ++ a.value.repalceLast(Simple(r_)))
            Deep(f, a, m, b__, r__)
        }
      } else if (!a.value.isEmpty) {
        a.value.head match {
          case Simple(d) =>
            val r_ = dappendR(m, r.init)
            Deep(f, Eval.later(a.value.tail), d, Eval.now(empty), r_)
          case Cmpd(f_, c_, r_) =>
            val r__ = dappendR(m, r.init)
            val b__ = Eval.later(c_.value.snoc(Simple(r_)))
            Deep(f, Eval.later(a.value.init), r_, b__, r__)
        }
      } else {
        Shallow(r) ++ Shallow(dappendR(m, r.init))
      }
  }

  private def repalceHead[B >: A](x: B): ImplicitCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.tail.cons(x))
    case Deep(f, a, m, b, r) => Deep(f.tail.cons(x), a, m, b, r)
  }

  private def repalceLast[B >: A](x: B): ImplicitCatenableDeque[B] = this match {
    case Shallow(d) => Shallow(d.init.snoc(x))
    case Deep(f, a, m, b, r) => Deep(f, a, m, b, r.init.snoc(x))
  }

}

object ImplicitCatenableDeque {
  val empty: ImplicitCatenableDeque[Nothing] = Shallow(BankersDeque.empty)

  private def dappendL[A](d1: BankersDeque[A], d2: BankersDeque[A]): BankersDeque[A] = {
    if (d1.isEmpty) d2 else dappendL(d1.init, d2.cons(d1.last))
  }

  private def dappendR[A](d1: BankersDeque[A], d2: BankersDeque[A]): BankersDeque[A] = {
    if (d2.isEmpty) d1 else dappendR(d1.snoc(d2.head), d2.tail)
  }

  private def share[A](f: BankersDeque[A], r: BankersDeque[A]): (BankersDeque[A], BankersDeque[A], BankersDeque[A]) = {
    val m = BankersDeque.empty.cons(r.head).cons(f.last)
    (f.init, m, r.tail)
  }

  case class Shallow[A](x: BankersDeque[A]) extends ImplicitCatenableDeque[A]
  case class Deep[A](f: BankersDeque[A],
                     a: Eval[ImplicitCatenableDeque[CmpdElem[A]]],
                     m: BankersDeque[A],
                     b: Eval[ImplicitCatenableDeque[CmpdElem[A]]],
                     r: BankersDeque[A])
    extends ImplicitCatenableDeque[A]

}

sealed trait CmpdElem[+A]
object CmpdElem {
  case class Simple[A](a: BankersDeque[A]) extends CmpdElem[A]
  case class Cmpd[A](f: BankersDeque[A], m: Eval[ImplicitCatenableDeque[CmpdElem[A]]], r: BankersDeque[A])
    extends CmpdElem[A]
}
