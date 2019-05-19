package lnc.kripke

import lnc.expr.Expr

object KripkeStructureBFS {

  def findPath(kripke: KripkeStructure, from: Set[Expr], to: Set[Expr]): Option[List[KripkeStructureNode]] = {
    val startNodes = kripke.nodes.values.filter(n => from.forall(n.terms.contains)).map(_.id).toList
    val goalNodes = kripke.nodes.values.filter(n => to.forall(n.terms.contains)).map(_.id).toSet

    def bfs(nodes: List[List[Int]], visited: Set[Int], goal: Set[Int]): Option[List[Int]] = nodes match {
      case Nil => None
      case path :: tail =>
        if (goal.contains(path.head)) Some(path.reverse)
        else {
          if (visited.contains(path.head)) {
            bfs(tail, visited, goal)
          } else {
            val newPaths = kripke.edges(path.head).map(_ :: path).toList
            bfs(tail ++ newPaths, visited + path.head, goal)
          }
        }
    }

    bfs(startNodes.map(List(_)), Set.empty, goalNodes).map(_.map(kripke.nodes.apply))
  }
}
