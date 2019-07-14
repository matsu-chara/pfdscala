package chapter03.redblack

sealed trait Color
case object R extends Color
case object B extends Color

sealed trait Tree[+E]
case object Empty extends Tree[Nothing]
case class Node[+E](color: Color, left: Tree[E], elem: E, right: Tree[E]) extends Tree[E]

object RedBlackTree {
  import Ordered._

  def member[E: Ordering](x: E, t: Tree[E]): Boolean = t match {
    case Empty => false
    case Node(_, a, y, b) =>
      if (x < y) member(x, a)
      else if (x > y) member(x, b)
      else true
  }

  def insert[E: Ordering](x: E, t: Tree[E]): Tree[E] = {
    ins(x, t).copy(color = B)
  }

  def balance[E: Ordering](color: Color, left: Tree[E], elem: E, right: Tree[E]): Node[E] = {
    (color, left, elem, right) match {
      case (B, Node(R, Node(R, a, x, b), y, c), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case (B, Node(R, a, x, Node(R, b, y, c)), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case (B, a, x, Node(R, b, y, Node(R, c, z, d))) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case (B, a, x, Node(R, Node(R, b, y, c), z, d)) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case _ => throw new IllegalArgumentException()
    }
  }

  def lbalance[E: Ordering](color: Color, left: Tree[E], elem: E, right: Tree[E]): Node[E] = {
    (color, left, elem, right) match {
      case (B, Node(R, Node(R, a, x, b), y, c), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case (B, Node(R, a, x, Node(R, b, y, c)), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case _ => throw new IllegalArgumentException()
    }
  }

  def rbalance[E: Ordering](color: Color, left: Tree[E], elem: E, right: Tree[E]): Node[E] = {
    (color, left, elem, right) match {
      case (B, a, x, Node(R, b, y, Node(R, c, z, d))) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case (B, a, x, Node(R, Node(R, b, y, c), z, d)) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
      case _ => throw new IllegalArgumentException()
    }
  }

  private def ins[E: Ordering](x: E, t: Tree[E]): Node[E] = t match {
    case Empty => Node(R, Empty, x, Empty)
    case n@Node(color, a, y, b) =>
      if (x < y) lbalance(color, ins(x, a), y, b)
      else if (x > y) rbalance(color, a, y, ins(x, b))
      else n
  }
}
