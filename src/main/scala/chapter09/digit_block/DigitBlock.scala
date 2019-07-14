package chapter09.digit_block

sealed trait DigitBlock
object DigitBlock {
  type Nat = List[DigitBlock]

  def zeros(num: Int, nat: Nat): Nat = (num, nat) match {
    case (_, Nil) => Nil
    case (0, blks) => blks
    case (i, Zeros(j) :: blks) => Zeros(i + j) :: blks
    case (i, blks) => Zeros(i) :: blks
  }

  def ones(num: Int, nat: Nat): Nat = (num, nat) match {
    case (0, blks) => blks
    case (i, Ones(j) :: blks) => Ones(i + j) :: blks
    case (i, blks) => Ones(i) :: blks
  }

  def inc(nat: Nat): Nat = nat match {
    case Nil => Ones(1) :: Nil
    case Zeros(i) :: blks => ones(1, zeros(i - 1, blks))
    case Ones(i) :: blks => Zeros(i) :: inc(blks)
  }

  def dec(nat: Nat): Nat = nat match {
    case Ones(i) :: blks => zeros(1, ones(i - 1, blks))
    case Zeros(i) :: blks => Ones(i) :: dec(blks)
    case _ => throw new NoSuchElementException
  }

  case class Zeros(len: Int) extends DigitBlock
  case class Ones(len: Int) extends DigitBlock
}
