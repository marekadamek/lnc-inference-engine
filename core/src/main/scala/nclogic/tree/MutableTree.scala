package nclogic.tree

class MutableTree[N](val node: TreeNode[N], var children: List[MutableTree[N]]) {


  def addChild(child: N): MutableTree[N] = {
    if (children.exists(_.node.value == child)) {
      throw new RuntimeException("child already added")
    }

    val childTree = new MutableTree(TreeNode(child), Nil)
    children = children ++ List(childTree)
    childTree
  }

  def getOrElseAddAndGetChildTree(child: N): MutableTree[N] = {
    children.find(_.node.value == child)
      .getOrElse(addChild(child))
  }

  def getPaths: List[List[N]] = {
    if (children.isEmpty) List(List(node.value))
    else {
      val tails = children.flatMap(_.getPaths)
      tails.map(node.value :: _)
    }
  }

  def print(): Unit = {
    print("", isTail = true)
  }

  private def print(prefix: String, isTail: Boolean): Unit = {
    println(prefix + (if (isTail) "└── " else "├── ") + node)

    for (i <- 0 until children.length - 1) {
      val childPrefix = prefix + (if (isTail) "    " else "│   ")
      children(i).print(childPrefix, isTail = false)
    }

    if (children.nonEmpty) {
      val lastChildPrefix = prefix + (if (isTail) "    " else "│   ")
      children.last.print(lastChildPrefix, isTail = true)
    }
  }

  def findSubtree(id: Int): Option[MutableTree[N]] = {
    if (id == node.id) Some(this)
    else children.find(c => c.findSubtree(id).isDefined)
  }

  def findNode(id: Int): Option[TreeNode[N]] = findSubtree(id).map(_.node)

  def getSuccessors(id: Int): List[TreeNode[N]] = {
    findSubtree(id)
      .map(_.children.map(_.node))
      .getOrElse(Nil)
  }
}


