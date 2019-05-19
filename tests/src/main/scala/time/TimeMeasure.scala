package time

case class TimeMeasure(nanoTime: Long) {
  private def normalize(v: Float) = BigDecimal(v).setScale(3, BigDecimal.RoundingMode.HALF_UP).toFloat

  lazy val microTime: Float = normalize(nanoTime / 1000f)
  lazy val millis: Float = normalize(microTime / 1000f)
  lazy val seconds: Float = normalize(millis / 1000f)
}
