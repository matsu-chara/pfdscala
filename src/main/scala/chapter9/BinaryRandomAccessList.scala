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
  def lookup[A](index: Int, list: List[Digit[A]]): A = list match {
    case Zero :: ts => lookup(index, ts)
    case One(t) :: ts => if (index < t.size) lookupTree(index, t) else lookup(index - t.size, ts)
    case _ => throw new IllegalStateException
  }

  private def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = {
    Node(t1.size + t2.size, t1, t2)
  }

  def update[A](index: Int, y: A, list: List[Digit[A]]): List[Digit[A]] = list match {
    case Zero :: ts => Zero :: update(index, y, ts)
    case One(t) :: ts =>
      if (index < t.size) One(updateTree(index, y, t)) :: ts else One(t) :: update(index - t.size, y, ts)
    case _ => throw new IllegalStateException
  }

  private def consTree[A](t: Tree[A], digits: List[Digit[A]]): List[Digit[A]] = {
    (t, digits) match {
      case (_, Nil) => One(t) :: Nil
      case (_, Zero :: ts) => One(t) :: ts
      case (t1, One(t2) :: ts) => Zero :: consTree(link(t1, t2), ts)
    }
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

  private def lookupTree[A](index: Int, tree: Tree[A]): A = tree match {
    case Leaf(x) => x
    case Node(w, t1, t2) =>
      if (index < w / 2) lookupTree(index, t1) else lookupTree(index - w / 2, t2)
  }

  private def updateTree[A](index: Int, y: A, tree: Tree[A]): Tree[A] = tree match {
    case Leaf(_) if index == 0 => Leaf(y)
    case Leaf(_) => throw new NoSuchElementException
    case Node(w, t1, t2) =>
      if (index < w / 2) Node(w, updateTree(index, y, t1), t2) else Node(w, t1, updateTree(index - w / 2, y, t2))
  }

}
