package chapter4

sealed trait Stream[+A]
case object Nil extends Stream[Nothing]
class Cons[A](val head: A, tl: => Stream[A]) extends Stream[A] {
  def tail: Stream[A] = tl
}

object Cons {
  def apply[A](head: A, tl: => Stream[A]): Cons[A] = Cons(head, tl)

  def append[A](a: Stream[A], b: Stream[A]): Stream[A] = (a, b) match {
    case (Nil, y) => y
    case (Cons(h, t), y) => Cons(h, append(t, y))
  }

  def take[A](n: Int, a: Stream[A]): Stream[A] = (n, a) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (i, Cons(h, t)) => Cons(h, take(i - 1, t))
  }

  def drop[A](n: Int, a: Stream[A]): Stream[A] = {
    (n, a) match {
      case (0, _) => Nil
      case (_, Nil) => Nil
      case (i, Cons(_, t)) => drop(i - 1, t)()
    }
  }

  def reverse[A](s: Stream[A]): Stream[A] = {
    def go(s2: Stream[A], acc: Stream[A]): Stream[A] = s2 match {
      case Nil => acc
      case Cons(h, t) => go(t, Cons(h, acc))
    }

    go(s, Nil)
  }

  def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] = {
    if (xs == Nil) {
      None
    } else {
      val cons = xs.asInstanceOf[Cons[A]]
      Some(cons.head, cons.tail)
    }
  }
}
