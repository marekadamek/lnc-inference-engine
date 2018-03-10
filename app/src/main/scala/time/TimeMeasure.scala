package time

case class TimeMeasure[R](nanoTime: Long, result: R) {
  lazy val microTime: Long = nanoTime / 1000
}
