package object time {

  def measureTime[R](block: => R): TimeMeasure[R] = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    TimeMeasure(t1 - t0, result)
  }

}
