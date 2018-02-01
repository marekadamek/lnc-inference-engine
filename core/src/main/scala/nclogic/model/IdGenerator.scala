package nclogic.model

object IdGenerator {
  private var values: Map[Class[_], Int] = Map.empty

  def next(key: Class[_]): Int = {
    val value = values.getOrElse(key, 0) + 1
    values = values.updated(key, value)
    value
  }
}
