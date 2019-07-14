package chapter05.paring

sealed abstract class Tree[E: Ordering] {

  import Tree._

  def findMin: E = this match {
    case Node(x, _) => x
    case Empty() => throw new NoSuchElementException()
  }

  def insert(x: E): Tree[E] = {
    merge(this, Node(x, Nil))
  }

  def deleteMin: Tree[E] = this match {
    case n: Node[_] => mergePairs(n.children)
    case Empty() => throw new NoSuchElementException()
  }
}
object Tree {
  import Ordered._

  def merge[E: Ordering](self: Tree[E], other: Tree[E]): Tree[E] = (self, other) match {
    case (h, Empty()) =>
      h
    case (Empty(), h) =>
      h
    case (h1@Node(x, hs1), h2@Node(y, hs2)) =>
      if (x <= y) {
        Node(x, h2 :: hs1)
      } else {
        Node(y, h1 :: hs2)
      }
  }

  private def mergePairs[E: Ordering](children: List[Tree[E]]): Tree[E] = children match {
    case Nil => Empty()
    case h1 :: Nil => h1
    case h1 :: h2 :: hs => merge(merge(h1, h2), mergePairs(hs))
  }

}
case class Node[E: Ordering](elem: E, children: List[Tree[E]]) extends Tree[E]
case class Empty[E: Ordering]() extends Tree[E]
