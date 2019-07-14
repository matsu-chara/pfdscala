package chapter02

import scala.language.higherKinds

trait StackOps[Stack[_]] {
  def empty[A]: Stack[A]
  def isEmpty[A](s: Stack[A]): Boolean
  def cons[A](a: A, b: Stack[A]): Stack[A]
  def head[A](s: Stack[A]): A
  def tail[A](s: Stack[A]): Stack[A]
  def concat[A](s1: Stack[A], s2: Stack[A]): Stack[A]
  def update[A](s: Stack[A], i: Int, a: A): Stack[A]
  def suffixes[A](s: Stack[A]): Stack[Stack[A]]
}

object ListImpl extends StackOps[List] {
  override def empty[A]: List[A] = List.empty

  override def isEmpty[A](s: List[A]): Boolean = s.isEmpty

  override def cons[A](a: A, s: List[A]): List[A] = a :: s

  override def head[A](s: List[A]): A = s.head

  override def tail[A](s: List[A]): List[A] = s.tail

  override def concat[A](s1: List[A], s2: List[A]): List[A] = s1 ++ s2

  override def update[A](s: List[A], i: Int, a: A): List[A] = s.updated(i, a)

  override def suffixes[A](s: List[A]): List[List[A]] = s.tails.toList
}


sealed trait MyList[+A]
object MyList {
  case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]
  case object Nil extends MyList[Nothing]
}

object ListImpl2 extends StackOps[MyList] {
  import MyList._

  override def empty[A]: MyList[A] = Nil

  override def isEmpty[A](s: MyList[A]): Boolean = s match {
    case Nil => true
    case _   => false
  }

  override def cons[A](a: A, b: MyList[A]): MyList[A] = Cons(a, b)

  override def head[A](s: MyList[A]): A = s match {
    case Nil        => throw new IllegalArgumentException()
    case Cons(a, _) => a
  }

  override def tail[A](s: MyList[A]): MyList[A] = s match {
    case Nil        => throw new IllegalArgumentException()
    case Cons(_, b) => b
  }

  override def concat[A](s1: MyList[A], s2: MyList[A]): MyList[A] = {
    if (isEmpty(s1)) s2 else Cons(head(s1), concat(tail(s1), s2))
  }

  override def update[A](s: MyList[A], i: Int, a: A): MyList[A] = {
    s match {
      case Cons(_, xs) if i <= 0 => Cons(a, xs)
      case Cons(x, xs) => Cons(x, update(xs, i-1, a))
      case Nil => throw new IllegalArgumentException()
    }
  }

  override def suffixes[A](s: MyList[A]): MyList[MyList[A]] = {
    s match {
      case Cons(_, Nil) => Cons(s, Nil)
      case Cons(_, xs) => Cons(s, suffixes(xs))
      case Nil => Nil
    }
  }
}
