package time

case class TimeMeasure(nanoTime: Long) {
  lazy val microTime: Float = nanoTime / 1000f
  lazy val millis: Float = microTime / 1000f
  lazy val seconds: Float = millis / 1000f
}
