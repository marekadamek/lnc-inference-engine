package nclogic.graph

trait Graph[N] {

  def addNode(node: N): GraphImpl[N]

  def addEdge(edge: Edge[N]): GraphImpl[N]

  def nodes: Set[N]

  def edges: Set[Edge[N]]

  def getSuccessors(node: N): Set[N]

}

case class Edge[N](from: N, to: N)

object Graph {
  override def finalize(): Unit = super.finalize()

  def empty[N]: Graph[N] = GraphImpl(Set.empty[N], Set.empty[Edge[N]])
}
