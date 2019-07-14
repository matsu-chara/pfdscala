package chapter09.lazy_digit

/**
  * use 0, 1,2 instead of 0,1 for number representation.
  * this representation is redundant. (222222 + 1 = 1111111, but 1111111 - 1 = 0111111
  *
  * this redundancy helps reducing computational complexity of inc,dec when using LazyList[Digit]
  *
  */
sealed trait Digit

object Digit {
  type Nat = LazyList[Digit]

  val LNil: LazyList[Digit] = LazyList.empty[Digit]

  def fromString(str: String): Nat = {
    LazyList.from(str.map {
      case '0' => Zero
      case '1' => One
      case '2' => Two
      case c => throw new IllegalArgumentException(s"invalid $c. use 1 or 2")
    })
  }

  def formatToString(n: Nat): String = {
    n.map {
      case Zero => '0'
      case One => '1'
      case Two => '2'
    }.mkString
  }

  def inc(n: Nat): Nat = n match {
    case LNil => One #:: LNil
    case Zero #:: ds => One #:: ds
    case One #:: ds => Two #:: ds
    case Two #:: ds => One #:: inc(ds)
  }

  def dec(n: Nat): Nat = n match {
    case One #:: LNil => LNil
    case One #:: ds => Zero #:: ds
    case Two #:: ds => One #:: ds
    case Zero #:: ds => One #:: dec(ds)
  }

  case object Zero extends Digit
  case object One extends Digit
  case object Two extends Digit
}
