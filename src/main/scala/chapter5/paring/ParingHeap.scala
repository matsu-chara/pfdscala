package chapter5.paring

sealed abstract class Tree[E: Ordering]
case class Node[E: Ordering](left: Tree[E], elem: E, right: Tree[E]) extends Tree[E]
case class Empty[E: Ordering]() extends Tree[E]

class ParingHeap {

}
