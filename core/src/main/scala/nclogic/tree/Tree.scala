package nclogic.tree

case class Tree[N](value: N, children: Set[Tree[N]]) {

  def getPaths(): Set[List[N]] = {
    if (children.isEmpty) Set(List(value))
    else {
      val tails = children.flatMap(_.getPaths())
      tails.map(value :: _)
    }
  }

}


