package chapter8.hood_melville_queue

import chapter8.hood_melville_queue.RotationState.{Done, Idle, Reversing}

case class HoodMelvilleQueue[+A](lenf: Int, f: List[A], state: RotationState[A], lenr: Int, r: List[A]) {
  def isEmpty: Boolean = {
    lenf == 0
  }

  def snoc[B >: A](x: B): HoodMelvilleQueue[B] = {
    HoodMelvilleQueue(lenf, f, state, lenr + 1, x :: r).check
  }

  def head: A = {
    if (f.isEmpty) throw new NoSuchElementException
    else f.head
  }

  def tail: HoodMelvilleQueue[A] = {
    if (f.isEmpty) throw new NoSuchElementException
    else HoodMelvilleQueue(lenf - 1, f, state.invalidate, lenr, r).check
  }

  private def check: HoodMelvilleQueue[A] = {
    if (lenr < lenf) {
      exec2
    } else {
      val newState = Reversing(0, f, Nil, r, Nil)
      HoodMelvilleQueue(lenf + lenr, f, newState, 0, Nil).exec2
    }
  }

  private def exec2: HoodMelvilleQueue[A] = {
    state.exec.exec match {
      case Done(newF) => HoodMelvilleQueue(lenf, newF, Idle(), lenr, r)
      case newState => HoodMelvilleQueue(lenf, f, newState, lenr, r)
    }
  }
}

object HoodMelvilleQueue {
  def empty[A]: HoodMelvilleQueue[A] = HoodMelvilleQueue[A](0, Nil, Idle(), 0, Nil)
}
