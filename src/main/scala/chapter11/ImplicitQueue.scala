package chapter11

import cats.Eval

sealed trait Digit[+A]

object Digit {
  case class One[A](a: A) extends Digit[A]
  case class Two[A](a1: A, a2: A) extends Digit[A]
  case object Zero extends Digit[Nothing]
}

sealed trait ImplicitQueue[+A] {

  import Digit._
  import ImplicitQueue._

  def isEmpty: Boolean = this match {
    case Shallow(Zero) => true
    case _ => false
  }

  def snoc[B >: A](y: B): ImplicitQueue[B] = this match {
    case Shallow(Zero) => Shallow(One(y))
    case Shallow(One(x)) => Deep(Two(x, y), Eval.later(empty), Zero)
    case Deep(f, m, Zero) => Deep(f, m, One(y))
    case Deep(f, m, One(x)) => Deep(f, Eval.later(m.value.snoc((x, y))), Zero)
    case _ => throw new IllegalStateException
  }

  def head: A = this match {
    case Shallow(Zero) => throw new NoSuchElementException
    case Shallow(One(x)) => x
    case Deep(One(x), _, _) => x
    case Deep(Two(x, _), _, _) => x
    case _ => throw new IllegalStateException
  }

  def tail: ImplicitQueue[A] = this match {
    case Shallow(Zero) => throw new NoSuchElementException
    case Shallow(One(_)) => empty
    case Deep(Two(_, y), m, r) => Deep(One(y), m, r)
    case Deep(One(_), q, r) =>
      if (q.value.isEmpty) Shallow(r)
      else {
        val (y, z) = q.value.head
        Deep(Two(y, z), Eval.later(q.value.tail), r)
      }
    case _ => throw new IllegalStateException
  }
}

object ImplicitQueue {

  import Digit._

  val empty = Shallow(Zero)
  case class Shallow[A](x: Digit[A]) extends ImplicitQueue[A]
  case class Deep[A](f: Digit[A], m: Eval[ImplicitQueue[(A, A)]], r: Digit[A]) extends ImplicitQueue[A]

}
