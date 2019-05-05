package lnc.utils

import scala.collection.mutable

object Memoize {

  def withCache[T, U](f: T => U) = {
    val cache = mutable.HashMap.empty[T, U]
    (t: T) => cache.getOrElseUpdate(t, f(t))
  }
}
