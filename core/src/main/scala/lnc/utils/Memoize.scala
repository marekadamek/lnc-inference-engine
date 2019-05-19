package lnc.utils

import scala.collection.mutable

object Memoize {

  /**
    * Provides memoization support for given function using underlying HashTable
    * @param f input functio
    * @tparam T type of input function parameter
    * @tparam U type of input function result
    * @return memoized function equivalent to input function
    */
  def withCache[T, U](f: T => U): T => U = {
    val cache = mutable.HashMap.empty[T, U]
    (t: T) => cache.getOrElseUpdate(t, f(t))
  }
}
