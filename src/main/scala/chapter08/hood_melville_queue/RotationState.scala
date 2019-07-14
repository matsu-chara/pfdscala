package chapter08.hood_melville_queue

sealed abstract class RotationState[+A] {

  import RotationState._

  /**
    * require |r| = |f| + 1
    */
  def exec: RotationState[A] = this match {
    case Reversing(ok, x :: f, f_, y :: r, r_) => Reversing(ok + 1, f, x :: f_, r, y :: r_)
    case Reversing(ok, Nil, f_, y :: Nil, r_) => Appending(ok, f_, y :: r_)
    case Appending(0, _, r_) => Done(r_)
    case Appending(ok, x :: f_, r_) => Appending(ok - 1, f_, x :: r_)
    case state => state
  }

  def invalidate: RotationState[A] = this match {
    case reversing@Reversing(ok, _, _, _, _) => reversing.copy(ok = ok - 1)
    case Appending(0, _, _ :: r_) => Done(r_)
    case appending@Appending(ok, _, _) => appending.copy(ok = ok - 1)
    case state => state
  }
}

object RotationState {
  def startRotation[A](f: List[A], r: List[A]): RotationState[A] = Reversing(0, f, Nil, r, Nil)

  case class Idle[A]() extends RotationState[A]
  case class Reversing[A](ok: Int, f: List[A], accF: List[A], r: List[A], accR: List[A]) extends RotationState[A]
  case class Appending[A](ok: Int, a: List[A], b: List[A]) extends RotationState[A]
  case class Done[A](value: List[A]) extends RotationState[A]
}
