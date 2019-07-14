package chapter05

case class Queue[A](out: List[A], in: List[A]) {
  // 不変条件: outがempty ならば inはempty

  def head: A = out.head // queue全体がemptyのときのみ例外を投げる。

  def tail: Queue[A] = this match {
    case Queue(Nil, _) => throw new NoSuchElementException
    case Queue(_ :: front, rear) => copy(out = front, in = rear).checkf
  }

  def snoc(x: A): Queue[A] = copy(out = out, in = x :: in).checkf

  private def checkf: Queue[A] = this match {
    case Queue(Nil, rear) => Queue(rear.reverse, Nil)
    case _ => this
  }
}

object Queue {
  def empty[A]: Queue[A] = Queue(Nil, Nil)
}
