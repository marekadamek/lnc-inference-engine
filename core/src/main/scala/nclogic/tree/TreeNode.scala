package nclogic.tree

import nclogic.model.IdGenerator

case class TreeNode[N](value: N) {
  val id: Int = IdGenerator.next(TreeNode.getClass)

  override def toString: String = id + ": " + value
}
