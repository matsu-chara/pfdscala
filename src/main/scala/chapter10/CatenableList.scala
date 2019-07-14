package chapter10

import cats.Eval
import chapter8.hood_melville_queue.HoodMelvilleQueue

sealed trait CatenableList[+A] {

  import CatenableList._

  def isEmpty: Boolean = this match {
    case E => true
    case _ => false
  }

  def cons[B >: A](x: B): CatenableList[B] = {
    C(x, Queue.empty) ++ this
  }

  def ++[B >: A](cat2: CatenableList[B]): CatenableList[B] = (this, cat2) match {
    case (xs, E) => xs
    case (E, ys) => ys
    case (xs, ys) => xs.link(Eval.now(ys))
  }

  private def link[B >: A](cat2: Eval[CatenableList[B]]): CatenableList[B] = this match {
    case C(x, q) => C(x, q.snoc(cat2))
    case _ => throw new IllegalArgumentException
  }

  def snoc[B >: A](x: B): CatenableList[B] = {
    this ++ C(x, Queue.empty)
  }

  def head: A = this match {
    case E => throw new NoSuchElementException
    case C(a, _) => a
  }

  def tail: CatenableList[A] = this match {
    case E => throw new NoSuchElementException
    case C(_, q) => if (q.isEmpty) E else linkAll(q).value
  }
}

object CatenableList {
  private type Q[A] = HoodMelvilleQueue[A]
  val empty: CatenableList[Nothing] = E
  private val Queue = HoodMelvilleQueue

  /** O(n) */
  private def linkAllSlow[A](q: Q[CatenableList[A]]): CatenableList[A] = {
    val t = q.head
    val q_ = q.tail
    if (q_.isEmpty) t else t.link(Eval.now(linkAllSlow(q_)))
  }

  private def linkAll[A](q: Q[Eval[CatenableList[A]]]): Eval[CatenableList[A]] = {
    for {
      t <- q.head
    } yield {
      val q_ = q.tail
      if (q_.isEmpty) t else t.link(linkAll(q_))
    }
  }

  case class C[A](a1: A, queue: Q[Eval[CatenableList[A]]]) extends CatenableList[A]
  case object E extends CatenableList[Nothing]
}
