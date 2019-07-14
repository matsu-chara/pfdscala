package chapter07

import cats.Eval

case class Tree[+A: Ordering](value: A, children: List[Tree[A]])
sealed abstract class Digit[+A: Ordering]
case class Zero[+A: Ordering]() extends Digit[A]
case class One[+A: Ordering](t: Tree[A]) extends Digit[A]

case class Schedule[+A: Ordering](value: List[LazyList[Digit[A]]])

case class ScheduledBinomialHeap[+A: Ordering](trees: LazyList[Digit[A]], schedule: Schedule[A])

object ScheduledBinomialHeap {

  import Ordered._

  private val LNil: LazyList[Nothing] = LazyList.empty

  def exec[A: Ordering](schedule: Schedule[A]): Schedule[A] = schedule.value match {
    case (One(_) #:: _) :: sched => Schedule(sched)
    case Zero() #:: job :: sched => Schedule(job :: sched)
  }

  def link[A: Ordering](t1: Tree[A], t2: Tree[A]): Tree[A] = {
    if (t1.value < t2.value) {
      Tree(t1.value, t2 :: t1.children)
    } else {
      Tree(t2.value, t1 :: t2.children)
    }
  }

  /** actually, There is no need to use delay evaluation. */
  def insTree[A: Ordering](tree: Tree[A], list: LazyList[Digit[A]]): Eval[LazyList[Digit[A]]] = Eval.defer {
    (tree, list) match {
      case (t, LNil) => Eval.now(One(t) #:: LNil)
      case (t, Zero() #:: ds) => Eval.now(One(t) #:: ds)
      case (t, One(t_) #:: ds) =>
        insTree(link(t, t_), ds).map { inserted =>
          Zero() #:: inserted
        }
    }
  }

  def insert[A: Ordering](elem: A, heap: ScheduledBinomialHeap[A]): ScheduledBinomialHeap[A] = {
    val ds_ = insTree(Tree(elem, Nil), heap.trees).value
    ScheduledBinomialHeap(ds_, exec(exec(Schedule(ds_ :: heap.schedule.value))))
  }

  def merge[A: Ordering](heap1: ScheduledBinomialHeap[A], heap2: ScheduledBinomialHeap[A]): ScheduledBinomialHeap[A] = {
    val ds = normalize(mrg(heap1.trees, heap2.trees))
    ScheduledBinomialHeap(ds, Schedule(Nil))
  }

  private def mrg[A: Ordering](digitLazy1: LazyList[Digit[A]], digitLazy2: LazyList[Digit[A]]): LazyList[Digit[A]] = {
    (digitLazy1, digitLazy2) match {
      case (_, LNil) => digitLazy1
      case (LNil, _) => digitLazy2
      case (Zero() #:: ds1, d #:: ds2) => d #:: mrg(ds1, ds2)
      case (d #:: ds1, Zero() #:: ds2) => d #:: mrg(ds1, ds2)
      case (One(t1) #:: ds1, One(t2) #:: ds2) =>
        Zero() #:: insTree(link(t1, t2), mrg(ds1, ds2)).value
    }
  }

  private def normalize[A: Ordering](digitLazy: LazyList[Digit[A]]): LazyList[Digit[A]] = digitLazy match {
    case LNil => LNil
    case _ #:: ds_ =>
      normalize(ds_) // evaluate all tail
      digitLazy
  }
}
