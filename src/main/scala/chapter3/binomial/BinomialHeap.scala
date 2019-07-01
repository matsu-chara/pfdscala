package chapter3.binomial

case class Tree[A: Ordering](rank: Int, value: A, children: List[Tree[A]])

case class Heap[A: Ordering](trees: List[Tree[A]])

object Heap {
  import Ordered._

  def link[A: Ordering](t1: Tree[A], t2: Tree[A]): Tree[A] = {
    if (t1.rank != t2.rank) throw new IllegalArgumentException() else ()

    (t1, t2) match {
      case (Tree(r1, v1, c1), Tree(_, v2, c2)) =>
        if (v1 <= v2) {
          Tree(r1 + 1, v1, t2 :: c1)
        } else {
          Tree(r1 + 1, v2, t1 :: c2)
        }
    }
  }

  def rank[A: Ordering](tree: Tree[A]): Int = tree.rank

  def insert[A: Ordering](elem: A, heap: Heap[A]): Heap[A] = {
    insTree(Tree(0, elem, Nil), heap)
  }

  def merge[A: Ordering](heap1: Heap[A], heap2: Heap[A]): Heap[A] = (heap1, heap2) match {
    case (_, Heap(Nil)) => heap1
    case (Heap(Nil), _) => heap2
    case (Heap(h1 :: t1), Heap(h2 :: t2)) =>
      if (h1.rank < h2.rank) Heap(h1 :: merge(Heap(t1), heap2).trees)
      else insTree(link(h1, h2), merge(Heap(t1), Heap(t2)))
  }

  def findMin[A: Ordering](heap: Heap[A]): A = heap match {
    case Heap(t :: Nil) =>
      t.value
    case Heap(t :: ts) =>
      val v2 = findMin(Heap(ts))
      if (t.value < v2) t.value else v2
  }

  def deleteMin[A: Ordering](heap: Heap[A]): Heap[A] = {
    val (Tree(_, _, ts1), ts2) = removeMinTree(heap)
    merge(Heap(ts1.reverse), ts2)
  }

  private def insTree[A: Ordering](tree: Tree[A], heap: Heap[A]): Heap[A] = heap match {
    case Heap(Nil) =>
      Heap(tree :: Nil)
    case Heap(head :: tail) =>
      if (tree.rank < head.rank) {
        Heap(tree :: heap.trees)
      } else {
        insTree(link(tree, head), Heap(tail))
      }
  }

  private def removeMinTree[A: Ordering](heap: Heap[A]): (Tree[A], Heap[A]) = heap match {
    case Heap(t :: Nil) => (t, Heap(Nil))
    case Heap(t1 :: ts1) =>
      val (t2, Heap(ts2)) = removeMinTree(Heap(ts1))
      if (t1.value <= t2.value) (t1, Heap(ts1)) else (t2, Heap(t1 :: ts2))
  }
}
