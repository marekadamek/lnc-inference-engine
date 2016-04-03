package nclogic.model

trait Node[T] {
  def state: T
  def successors: Set[_ <: Node[T]]

  override def equals(o: Any) = o match {
    case other: Node[T] => state == other.state
    case _ => false
  }
}

case class LazyNode[T](state: T, private val getSuccessors: () => Set[LazyNode[T]]) extends Node[T] {
  lazy val successors = getSuccessors()
}

