package chapter04

object Memoizer {
  def $[X, Y](f: X => Y): X => Y = {
    val cache = scala.collection.mutable.Map[X, Y]()
    x => cache.getOrElseUpdate(x, f(x))
  }
}
