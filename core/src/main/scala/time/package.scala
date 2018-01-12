package object time {

  def measureTime[R](block: => R): TimeMeasure[R] = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    TimeMeasure(t1 - t0, result)
  }
}
