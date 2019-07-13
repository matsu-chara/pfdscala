package chapter8.hood_melville_queue

/**
  * this class is invalid on calling tail during reversing/appending situation
  */
sealed abstract class NoDropRotationState[A] {

  import NoDropRotationState._

  /**
    * require |r| = |f| + 1
    */
  def exec: NoDropRotationState[A] = this match {
    case Reversing(x :: f, f_, y :: r, r_) => Reversing(f, x :: f_, r, y :: r_)
    case Reversing(Nil, f_, y :: Nil, r_) => Appending(f_, y :: r_)
    case Appending(x :: f_, r_) => Appending(f_, x :: r_)
    case Appending(Nil, r_) => Done(r_)
    case Done(_) => throw new IllegalStateException
  }
}
object NoDropRotationState {
  def startRotation[A](f: List[A], r: List[A]): NoDropRotationState[A] = Reversing(f, Nil, r, Nil)

  case class Reversing[A](f: List[A], accF: List[A], r: List[A], accR: List[A]) extends NoDropRotationState[A]
  case class Appending[A](a: List[A], b: List[A]) extends NoDropRotationState[A]
  case class Done[A](value: List[A]) extends NoDropRotationState[A]
}
