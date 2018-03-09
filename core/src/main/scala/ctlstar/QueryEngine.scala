//package ctlstar
//
//import nclogic.LncInferenceEngine
//import nclogic.model.HistoryGraph
//import nclogic.model.expr.{Expr, False}
//
//object QueryEngine {
//
//  def runQuery(query: CtlQuery, formula: Expr): Boolean =
//    runQuery(query, LncInferenceEngine.getHistoryGraph(formula))
//
//  def runQuery(query: CtlQuery, graph: HistoryGraph): Boolean = {
//    val paths = getAllPaths(graph)
//    query.ctlOp match {
//      case ForAll => paths.forall(p => checkPath(p, query.ltlOp))
//      case Exists => paths.exists(p => checkPath(p, query.ltlOp))
//    }
//  }
//
//  def checkPath(path: List[Expr], ltlOp: LtlOperator): Boolean = ltlOp match {
//    case Globally(e) => path.forall(p => Expr.and(p, e).simplify != False)
//    case Finally(e) => path.exists(p => Expr.and(p, e).simplify != False)
//    //todo: the rest of ltl operators
//    case _ => false
//  }
//
//  def getAllPaths(graph: HistoryGraph): List[List[Expr]] = {
//    def getAllPaths(paths: List[List[Expr]], graph: HistoryGraph, acc: List[List[Expr]]): List[List[Expr]] = paths match {
//      case Nil => acc
//      case p :: ps =>
//        val successors = graph.getSuccessors(p.last)
//        if (successors.isEmpty) getAllPaths(ps, graph, p :: acc)
//        else {
//          val nextNodes = successors.filterNot(p.contains).toList
//          nextNodes.map(n => p ++ List(n)) match {
//            case Nil => getAllPaths(ps, graph, p :: acc)
//            case newPaths => getAllPaths(newPaths ++ ps, graph, acc)
//          }
//        }
//    }
//
//    val startPaths = graph.startNodes.map(List(_))
//    getAllPaths(startPaths, graph, Nil)
//  }
//
////  def buildTree(node: Expr, graph: HistoryGraph, used: List[Expr]): Tree[Expr] = {
////    val children = graph.graph.getSuccessors(node)
////      .filterNot(used.contains)
////      .map(child => buildTree(child, graph, child :: used))
////
////    Tree(node, children.toList)
////  }
//}
