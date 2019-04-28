package object time {

  def measureTime[R](block: => R): (R, TimeMeasure) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, TimeMeasure(t1 - t0))
  }

}
