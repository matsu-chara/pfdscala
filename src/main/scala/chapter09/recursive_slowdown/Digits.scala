package chapter09.recursive_slowdown

sealed trait Digits

object Digits {
  type Nat = List[Digits]

  def inc(n: Nat): Nat = fixup(simpleinc(n))

  private def fixup(n: Nat): Nat = n match {
    case Two :: ds => Zero :: simpleinc(ds)
    case Ones(i) :: Two :: ds => Ones(i) :: Zero :: simpleinc(ds)
    case ds => ds
  }

  private def simpleinc(n: Nat): Nat = n match {
    case Nil => Ones(1) :: Nil
    case Zero :: ds => ones(1, ds)
    case Ones(i) :: ds => Two :: ones(i - 1, ds)
  }

  private def ones(num: Int, nat: Nat): Nat = (num, nat) match {
    case (0, ds) => ds
    case (i, Ones(j) :: ds) => Ones(i + j) :: ds
    case (i, ds) => Ones(i) :: ds
  }

  case class Ones(len: Int) extends Digits // yellow
  case object Zero extends Digits // green
  case object Two extends Digits // red
}
