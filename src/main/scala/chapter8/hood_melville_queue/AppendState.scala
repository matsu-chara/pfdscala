package chapter8.hood_melville_queue

sealed abstract class AppendState[A] {

  import AppendState._

  def exec: AppendState[A] = this match {
    case Reversing(x :: xs, xs_, ys) => Reversing(xs, x :: xs_, ys)
    case Reversing(Nil, xs_, ys) => Appending(xs_, ys)
    case Appending(x :: xs_, ys) => Appending(xs_, x :: ys)
    case Appending(Nil, ys) => Done(ys)
    case Done(_) => throw new IllegalStateException
  }
}
object AppendState {
  def startAppend[A](xs: List[A], ys: List[A]): AppendState[A] = Reversing(xs, Nil, ys)

  case class Reversing[A](a: List[A], b: List[A], c: List[A]) extends AppendState[A]
  case class Appending[A](a: List[A], b: List[A]) extends AppendState[A]
  case class Done[A](value: List[A]) extends AppendState[A]
}
