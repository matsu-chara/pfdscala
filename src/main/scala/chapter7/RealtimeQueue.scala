package chapter7

/**
  * @param s scuedule
  */
case class RealtimeQueue[+A](f: LazyList[A], r: List[A], s: LazyList[A]) {

  import RealtimeQueue._

  def isEmpty: Boolean = if (f.isEmpty) true else false

  def snoc[B >: A](x: B): RealtimeQueue[B] = {
    RealtimeQueue(f, x :: r, s).exec
  }

  /**
    * keep requirement. |s| = |f| - |r| and |f| > |r|
    *
    * @note call this when "add to rear" or "drop from front" |s| = |f| - |r| + 1
    */
  private def exec: RealtimeQueue[A] = this match {
    case RealtimeQueue(_f, _r, _ #:: _s) =>
      RealtimeQueue(_f, _r, _s) // eval front via s. and drop front s (because it was already evaluated)
    case RealtimeQueue(_f, _r, LNil) =>
      val __f = RealtimeQueue.rotate(_f, _r, LNil)
      RealtimeQueue(__f, Nil, __f)
  }

  def head: A = {
    if (f.isEmpty) throw new NoSuchElementException
    else {
      f.head
    }
  }

  def tail: RealtimeQueue[A] = {
    if (f.isEmpty) throw new NoSuchElementException
    else {
      RealtimeQueue(f.tail, r, s).exec
    }
  }
}

object RealtimeQueue {
  def empty[A]: RealtimeQueue[A] = RealtimeQueue[A](LNil, Nil, LNil)

  private val LNil = LazyList.empty[Nothing]

  @deprecated
  private def rotate0[A](xs: LazyList[A], ys: List[A], acc: LazyList[A]): LazyList[A] = {
    xs ++ ys.reverse ++ acc // |ys| = |xs| + 1
  }

  private def rotate[A](xs: LazyList[A], ys: List[A], acc: LazyList[A]): LazyList[A] = (xs, ys, acc) match {
    case (LNil, y :: _, a) => // (y #:: _) if xs is empty, then length of ys is 1
      y #:: a
    case (x #:: _xs, y :: _ys, a) =>
      x #:: rotate(_xs, _ys, y #:: a)
  }
}
