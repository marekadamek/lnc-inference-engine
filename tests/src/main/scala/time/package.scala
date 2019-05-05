package object time {

  val log = false

  def measureTime[R](block: => R): (R, TimeMeasure) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, TimeMeasure(t1 - t0))
  }

  def timeLog[R](label: String)(block: => R): R = {
    if (log) {
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()

      println(s"Time log ($label): ${TimeMeasure(t1 - t0).seconds}")
      result
    }
    else {
      block
    }

  }

}
