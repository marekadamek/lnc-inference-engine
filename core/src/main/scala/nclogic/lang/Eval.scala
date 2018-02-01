//package nclogic.lang
//
//import nclogic.graph.Graph
//import nclogic.model.Formula
//import nclogic.model.expr.{And, Expr, True}
//
//
//object Eval {
//
//  private def evalExpr(e: Expr, graph: Graph[Expr]): Unit = {
//    val stop = e.getTerms.contains(Terminate)
//
//    if (stop) ()
//    else {
//      e.getTerms
//        .filter(_.isInstanceOf[Block])
//        .map(_.asInstanceOf[Block])
//        .foreach(_.execute())
//
//      val next = graph.getSuccessors(e).headOption.getOrElse(e)
//      // todo zle to jet
//      val toEval = if (next != e) next else Expr.and(Terminate, e)
//      evalExpr(toEval, graph)
//    }
//  }
//
//  def eval(formula: Formula): Unit = {
//    val graph = formula.historyGraph.graph
//    val expr = graph.getSuccessors(True).head
//
//    evalExpr(expr, graph)
//  }
//}
