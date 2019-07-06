package chapter5.spray

sealed abstract class Tree[E: Ordering]
case class Node[E: Ordering](left: Tree[E], elem: E, right: Tree[E]) extends Tree[E]
case class Empty[E: Ordering]() extends Tree[E]

object SprayHeap {
  import Ordered._

  def insert[E: Ordering](e: E, t: Tree[E]): Tree[E] = {
    Node(smaller(e, t), e, bigger2(e, t))
  }

  def findMin[E: Ordering](t: Tree[E]): E = t match {
    case Node(Empty(), x, _) => x
    case Node(a, _, _) => findMin(a)
  }

  def deleteMin[E: Ordering](t: Tree[E]): Tree[E] = t match {
    case Node(Empty(), _, b) => b
    case Node(Node(Empty(), x, b), y, c) => Node(b, y, c)
    case Node(Node(a, x, b), y, c) => Node(deleteMin(a), x, Node(b, y, c))
  }

  private def smaller[E: Ordering](pivot: E, t: Tree[E]): Tree[E] = t match {
    case Empty() => Empty()
    case Node(a, x, b) =>
      if (x > pivot) smaller(pivot, b) else Node(smaller(pivot, a), x, b)
  }

  private def bigger[E: Ordering](pivot: E, t: Tree[E]): Tree[E] = t match {
    case Empty() => Empty()
    case Node(a, x, b) =>
      if (x <= pivot) bigger(pivot, b) else Node(bigger(pivot, a), x, b)
  }

  private def bigger2[E: Ordering](pivot: E, t: Tree[E]): Tree[E] = t match {
    case Empty() =>
      Empty()
    case Node(a, x, b) =>
      if (x <= pivot) {
        bigger(pivot, b)
      } else {
        a match {
          case Empty() =>
            Node(Empty(), x, b)
          case Node(a1, y, a2) =>
            if (y <= pivot) {
              Node(bigger(pivot, a2), x, b)
            } else {
              Node(bigger(pivot, a1), y, Node(a2, x, b))
            }
        }
      }
  }

  // return (smaller, bigger)
  private def partition[E: Ordering](pivot: E, t: Tree[E]): (Tree[E], Tree[E]) = t match {
    case Empty() =>
      (Empty(), Empty())
    case Node(a, x, b) =>
      if (x <= pivot) {
        b match {
          case Empty() =>
            (t, Empty())
          case Node(b1, y, b2) =>
            if (y <= pivot) {
              val (small, big) = partition(pivot, b2)
              (Node(Node(a, x, b1), y, small), big)
            } else {
              val (small, big) = partition(pivot, b1)
              (Node(a, x, small), Node(big, y, b2))
            }
        }
      } else {
        a match {
          case Empty() =>
            (Empty(), t)
          case Node(a1, y, a2) =>
            if (y <= pivot) {
              val (small, big) = partition(pivot, a2)
              (Node(a1, y, small), Node(big, x, b))
            } else {
              val (small, big) = partition(pivot, a1)
              (small, Node(big, y, Node(a2, x, b)))
            }
        }
      }
  }

}
