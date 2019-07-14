package chapter10

sealed trait Seq[+A] {

  import Seq._

  def size: Int = this match {
    case Nil => 0
    case Zero(tail) => 2 * tail.size
    case One(_, tail) => 2 * tail.size
  }

  def cons[B >: A](x: B): Seq[B] = this match {
    case Nil => One(x, Nil)
    case Zero(ps) => One(x, ps)
    case One(y, ps) => Zero(ps.cons((x, y)))
  }

  def head: A = uncons._1

  def tail: Seq[A] = uncons._2

  def lookup(index: Int): A = (index, this) match {
    case (0, One(x, _)) => x
    case (i, One(_, ps)) => Zero(ps).lookup(i - 1)
    case (i, Zero(ps)) =>
      val (x, y) = ps.lookup(i / 2)
      if (i % 2 == 0) x else y
    case _ => throw new NoSuchElementException
  }

  /** O(log^2 n) */
  def updateLogLog[B >: A](index: Int, e: B): Seq[B] = (index, this) match {
    case (0, One(_, ps)) => One(e, ps)
    case (i, One(x, ps)) => Zero(ps).update(i - 1, e).cons(x)
    case (i, Zero(ps)) =>
      val (x, y) = ps.lookup(i / 2)
      val p = if (i % 2 == 0) (e, y) else (x, e)
      Zero(ps.update(i - 1, p))
    case _ => throw new NoSuchElementException
  }

  def update[B >: A](index: Int, y: B): Seq[B] = fupdate(_ => y, index)

  def fupdate[B >: A](f: A => B, index: Int): Seq[B] = (index, this) match {
    case (0, One(x, ps)) => One(f(x), ps)
    case (i, One(x, ps)) => Zero(ps).fupdate(f, i - 1).cons(x)
    case (i, Zero(ps)) =>
      Zero(ps.fupdate({ case (x, y) => if (i % 2 == 0) (f(x), y) else (x, f(y)) }, index / 2))
    case _ => throw new NoSuchElementException
  }

  private def uncons: (A, Seq[A]) = this match {
    case Nil => throw new NoSuchElementException
    case One(x, Nil) => (x, Nil)
    case One(x, ps) => (x, Zero(ps))
    case Zero(ps) =>
      val ((x, y), ps_) = ps.uncons
      (x, One(y, ps_))
  }
}

object Seq {
  case class Zero[A](tl: Seq[(A, A)]) extends Seq[A]
  case class One[A](hd: A, tl: Seq[(A, A)]) extends Seq[A]
  case object Nil extends Seq[Nothing]
}
