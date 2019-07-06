package chapter6

case class Queue[A](lenf: Int, f: LazyList[A], lenr: Int, r: LazyList[A]) {

  import Queue._

  def snoc(x: A): Queue[A] = {
    check(Queue(lenf, f, lenr + 1, x #:: r))
  }

  def head: A = {
    if (f.isEmpty) {
      throw new NoSuchElementException()
    } else {
      f.head
    }
  }

  def tail: Queue[A] = {
    if (f.isEmpty) {
      throw new NoSuchElementException()
    } else {
      check(Queue(lenf - 1, f.tail, lenr, r))
    }
  }

  def isEmpty: Boolean = lenf == 0
}

object Queue {
  def empty[A] = Queue(0, LazyList.empty, 0, LazyList.empty)

  def check[A](q: Queue[A]): Queue[A] = {
    if (q.lenr < q.lenf) q else Queue(q.lenf + q.lenr, q.f ++ q.r.reverse, 0, LazyList.empty)
  }
}
