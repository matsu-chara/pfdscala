package chapter9.one_two

/**
  * use 1,2 instead of 0,1 for number representation.
  *
  * (16)_10 is (2111)_2 instead of (00001)_2
  */
sealed trait Digit {}

object Digit {
  type Nat = List[Digit]

  def dec(n: Nat): Nat = n match {
    case One :: Nil => Nil
    case One :: ds => Two :: dec(ds)
    case Two :: ds => One :: ds
    case _ => throw new NoSuchElementException
  }

  def add(num1: Nat, num2: Nat): Nat = (num1, num2) match {
    case (n1, Nil) => n1
    case (Nil, n2) => n2
    case (One :: ns1, One :: ns2) => Two :: add(ns1, ns2)
    case (One :: ns1, Two :: ns2) => One :: inc(add(ns1, ns2))
    case (Two :: ns1, One :: ns2) => One :: inc(add(ns1, ns2))
    case (Two :: ns1, Two :: ns2) => One :: inc(inc(add(ns1, ns2)))
  }

  def fromString(str: String): Nat = {
    str.map {
      case '1' => One
      case '2' => Two
      case c => throw new IllegalArgumentException(s"invalid $c. use 1 or 2")
    }.toList
  }

  def formatToString(n: Nat): String = {
    n.map {
      case One => '1'
      case Two => '2'
    }.mkString
  }

  def inc(n: Nat): Nat = n match {
    case Nil => One :: Nil
    case One :: ds => Two :: ds
    case Two :: ds => One :: inc(ds)
  }

  case object One extends Digit
  case object Two extends Digit
}
