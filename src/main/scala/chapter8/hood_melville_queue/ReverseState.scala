package chapter8.hood_melville_queue

sealed abstract class ReverseState[A] {

  import ReverseState._

  def exec: ReverseState[A] = this match {
    case Working(x :: xs, xs_) => Working(xs, x :: xs_)
    case Working(Nil, xs_) => Done(xs_)
    case Done(_) => throw new IllegalStateException
  }
}
object ReverseState {
  def startReverse[A](xs: List[A]): ReverseState[A] = Working(xs, Nil)

  case class Working[A](a: List[A], b: List[A]) extends ReverseState[A]
  case class Done[A](value: List[A]) extends ReverseState[A]
}
