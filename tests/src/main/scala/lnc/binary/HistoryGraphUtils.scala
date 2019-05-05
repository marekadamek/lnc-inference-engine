//package nclogic.model
//
//import nclogic.model.expr.Expr
//
//
//object HistoryGraphUtils {
//  def getCycles(graph: HistoryGraph, start: Expr): List[List[Expr]] = {
//    def loop(todo: List[List[Expr]], cycles: List[List[Expr]]): List[List[Expr]] = todo match {
//      case Nil => cycles
//      case curr :: rest =>
//        val newPaths = graph.getSuccessors(curr.head)
//          .filterNot(s => s != start && curr.contains(s))
//          .map(_ :: curr)
//
//        val (newAcc, newTodo) = newPaths.partition(_.head == start)
//        val newCycles = newAcc.map(c => c.drop(1).reverse)
//        loop(rest ++ newTodo, (cycles ++ newCycles).distinct)
//    }
//
//    loop(List(List(start)), Nil)
//  }
//}