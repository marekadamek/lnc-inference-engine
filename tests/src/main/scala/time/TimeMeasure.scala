package time

case class TimeMeasure[R](nanoTime: Long, result: R) {
  lazy val microTime: Long = nanoTime / 1000
  lazy val millis: Long = microTime / 1000
}
