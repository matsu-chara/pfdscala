package chapter3

sealed abstract class Heap[T] {
  val rank: Int
}
object Heap {
  case class Empty[T]() extends Heap[T] {
    override val rank: Int = 0
  }
  case class Node[T](rank: Int, value: T, left: Heap[T], right: Heap[T]) extends Heap[T]
}

trait HeapOps {
  def empty[A: Ordering]: Heap[A]

  def isEmpty[A: Ordering](h: Heap[A]): Boolean

  def insert[A: Ordering](elem: A, h: Heap[A]): Heap[A]

  def merge[A: Ordering](l: Heap[A], r: Heap[A]): Heap[A]

  def findMin[A: Ordering](h: Heap[A]): A

  def deleteMin[A: Ordering](h: Heap[A]): Heap[A]
}

object HeapImpl extends HeapOps {
  import Ordered._
  import Heap._

  override def empty[A: Ordering]: Heap[A] = Empty[A]()

  override def isEmpty[A: Ordering](h: Heap[A]): Boolean = {
    h == Empty[A]()
  }

  override def insert[A: Ordering](elem: A, h: Heap[A]): Heap[A] = {
    merge(Node(1, elem, Empty(), Empty()), h)
  }

  def insert2[A: Ordering](elem: A, h: Heap[A]): Heap[A] = {
    val target = Node(1, elem, Empty(), Empty())

    h match {
      case Empty() =>
        target
      case Node(_, v, l, r) =>
        if (elem <= v) {
          makeNode(v, insert2(elem, l), r)
        } else {
          makeNode(v, l, insert2(elem, r))
        }

    }
  }

  override def merge[A: Ordering](l: Heap[A], r: Heap[A]): Heap[A] = (l, r) match {
    case (left, Empty()) => left
    case (Empty(), right) => right
    case (Node(_, v1, l1, r1), Node(_, v2, l2, r2)) =>
      if (v1 <= v2) {
        makeNode(v1, l1, merge(r1, r))
      }
      else {
        makeNode(v2, l2, merge(l, r2))
      }
  }

  override def findMin[A: Ordering](h: Heap[A]): A = {
    h match {
      case Node(_, v, _, _) => v
      case Empty() => throw new NoSuchElementException()
    }
  }

  override def deleteMin[A: Ordering](h: Heap[A]): Heap[A] = {
    h match {
      case Node(_, _, left, right) => merge(left, right)
      case Empty() => throw new NoSuchElementException()
    }

  }

  def fromList[A: Ordering](list: List[A]): Heap[A] = list match {
    case head :: tail =>
      val target = Node(1, head, Empty(), Empty())
      merge(target, fromList(tail))
    case Nil =>
      Empty()
  }

  private def makeNode[A: Ordering](value: A, left: Heap[A], right: Heap[A]): Heap[A] = {
    if (left.rank >= right.rank) Node(right.rank + 1, value, left, right)
    else Node(left.rank + 1, value, right, left)
  }
}
