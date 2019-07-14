package chapter10

import cats.Eval

/**
  * lenfm = |f| + |m|
  * lenr < lenfm
  */
sealed trait BootstrappedQueue[+A]
object BootstrappedQueue {
  def snoc[A](queue: BootstrappedQueue[A], x: A): BootstrappedQueue[A] = queue match {
    case E => Q(1, x :: Nil, E, 0, Nil)
    case Q(lenfm, f, m, lenr, r) => checkQ(lenfm, f, m, lenr + 1, x :: r)
  }

  def head[A](queue: BootstrappedQueue[A]): A = queue match {
    case E => throw new NoSuchElementException
    case Q(_, x :: _, _, _, _) => x
    case _ => throw new IllegalStateException
  }

  def tail[A](queue: BootstrappedQueue[A]): BootstrappedQueue[A] = queue match {
    case E => throw new NoSuchElementException
    case Q(lenfm, _ :: f, m, lenr, r) => checkQ(lenfm - 1, f, m, lenr + 1, r)
    case _ => throw new IllegalStateException
  }

  private def checkQ[A](lenfm: Int, f: List[A], m: BootstrappedQueue[Eval[List[A]]], lenr: Int, r: List[A]): BootstrappedQueue[A] = {
    if (lenr <= lenfm) {
      checkF(lenfm, f, m, lenr, r)
    } else {
      checkF(lenfm + lenr, f, snoc(m, Eval.later(r.reverse)), 0, Nil)
    }
  }

  private def checkF[A](lenfm: Int, f: List[A], m: BootstrappedQueue[Eval[List[A]]], lenr: Int, r: List[A]): BootstrappedQueue[A] = {
    (f, m) match {
      case (Nil, E) => E
      case (Nil, _) => Q(lenfm, head(m).value, tail(m), lenr, r)
      case _ => Q(lenfm, f, m, lenr, r)
    }
  }

  case class Q[A](lenfm: Int, f: List[A], m: BootstrappedQueue[Eval[List[A]]], lenr: Int, r: List[A]) extends BootstrappedQueue[A]
  case object E extends BootstrappedQueue[Nothing]
}
