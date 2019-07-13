package chapter9

sealed trait Tree[+A] {
  def size: Int
}
case class Leaf[+A](value: A) extends Tree[A] {
  def size: Int = 1
}
case class Node[+A](size: Int, left: Tree[A], right: Tree[A]) extends Tree[A]

sealed trait Digit[+A]
case object Zero extends Digit[Nothing]
case class One[A](tree: Tree[A]) extends Digit[A]

case class BinaryRandomAccessList[A](ts: List[Digit[A]]) {

  import BinaryRandomAccessList._

  def cons(x: A): BinaryRandomAccessList[A] = {
    BinaryRandomAccessList(consTree(Leaf(x), ts))
  }

  def head: A = {
    val Leaf(v) = unconsTree(ts)._1
    v
  }

  def tail: List[Digit[A]] = unconsTree(ts)._2

}

object BinaryRandomAccessList {
  private def consTree[A](t: Tree[A], digits: List[Digit[A]]): List[Digit[A]] = {
    (t, digits) match {
      case (_, Nil) => One(t) :: Nil
      case (_, Zero :: ts) => One(t) :: ts
      case (t1, One(t2) :: ts) => Zero :: consTree(link(t1, t2), ts)
    }
  }

  private def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = {
    Node(t1.size + t2.size, t1, t2)
  }

  private def unconsTree[A](digits: List[Digit[A]]): (Tree[A], List[Digit[A]]) = {
    digits match {
      case One(t) :: Nil => (t, Nil)
      case One(t) :: ts => (t, ts)
      case Zero :: ts =>
        val (Node(_, t1, t2), ts_) = unconsTree(ts)
        (t1, One(t2) :: ts_)
      case Nil => throw new NoSuchElementException
    }
  }

}
