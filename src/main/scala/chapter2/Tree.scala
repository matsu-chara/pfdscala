package chapter2

sealed abstract class Tree[A: Ordering]
object Tree {
  case class Node[A: Ordering](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
  case class Empty[A: Ordering]() extends Tree[A]
}

trait SetOps[Set[_]] {
  def empty[A: Ordering]: Set[A]

  def insert[A: Ordering](e: A, s: Set[A]): Set[A]

  def member[A: Ordering](e: A, s: Set[A]): Boolean
}

object SetImpl extends SetOps[Tree] {
  import chapter2.Tree._
  import Ordered._

  override def empty[A: Ordering]: Tree[A] = Empty()

  override def insert[A: Ordering](e: A, s: Tree[A]): Tree[A] = s match {
    case Empty()                               => Node(Empty(), e, Empty())
    case Node(left, value, right) if value < e => Node(insert(e, left), value, right)
    case Node(left, value, right) if value > e => Node(left, value, insert(e, right))
    case Node(_, _, _)                         => s
  }

  override def member[A: Ordering](e: A, s: Tree[A]): Boolean = s match {
    case Empty()                            => false
    case Node(left, value, _) if value < e  => member(e, left)
    case Node(_, value, right) if value > e => member(e, right)
    case Node(_, _, _)                  => true
  }
}
