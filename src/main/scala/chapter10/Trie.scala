package chapter10

import chapter10.Trie.MyMap

case class Trie[A](enabled: Option[A], map: MyMap[Trie[A]]) {
  def lookup(str: String): A = lookup_(str.toList)

  def bind(str: String, x: A): Trie[A] = bind_(str.toList, x)

  private def lookup_(chars: List[Char]): A = (chars, this) match {
    case (Nil, Trie(None, _)) => throw new NoSuchElementException
    case (Nil, Trie(Some(x), _)) => x
    case (k :: ks, Trie(v, m)) => m(k).lookup_(ks)
  }

  private def bind_(chars: List[Char], x: A): Trie[A] = (chars, this) match {
    case (Nil, Trie(_, m)) => Trie(Some(x), m)
    case (k :: ks, Trie(v, m)) =>
      val t = m.getOrElse(k, Trie.empty)
      val t_ = t.bind_(ks, x)
      Trie(v, m.updated(k, t_))
  }
}

object Trie {
  type MyMap[A] = Map[Char, A]
  val MyMap: Map.type = Map

  def empty[A]: Trie[A] = Trie(None, MyMap.empty)
}
