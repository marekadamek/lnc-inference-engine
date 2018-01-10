package nclogic.graph

case class GraphImpl[N](nodes: Set[N] = Set.empty[N], edges: Set[Edge[N]] = Set.empty[Edge[N]]) extends Graph[N] {
  def addNode(node: N) = GraphImpl(nodes + node, edges)

  def addEdge(edge: Edge[N]) = {
    val g = addNode(edge.from).addNode(edge.to)
    GraphImpl(g.nodes, g.edges + edge)
  }

  def getSuccessors(node: N) = edges.filter(_.from == node).map(_.to)
}

