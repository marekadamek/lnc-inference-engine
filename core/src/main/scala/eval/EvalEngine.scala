//package eval
//
//import nclogic.model.expr._
//import nclogic.tree.MutableTree
//
//object EvalEngine {
//
//  def eval(formula: Expr): Unit = {
//    val root = HistoryTree(formula).root
//    root.print()
//    def eval(trees: List[MutableTree[Expr]]): Unit = trees match {
//      case Nil => eval(List(root))
//      case t :: ts =>
//        val e  = t.node.value
//        //print("evaluating: " + e)
//        val result = evalExpr(List(e))
//        //println(" result " + result)
//        result match {
//          case True =>
//            val hasCut = t.node.value.getTerms.contains(Cut)
//            if (hasCut) eval(t.children)
//            else eval(t.children ++ ts)
//          case False => eval(ts)
//          case Terminate => ()
//        }
//    }
//
//    eval(Nil)
//  }
//
//  def evalExpr(es: List[Expr]): Expr = es match {
//      case Nil => True
//      case e :: tail => e match {
//        case Terminate => Terminate
//        case And(x) => evalExpr(x ++ tail)
//        case Block(_, block) =>
//          block()
//          evalExpr(tail)
//        case Cond(_, cond) =>
//          if (cond()) evalExpr(tail)
//          else False
//        case Neg(Cond(_, cond)) =>
//          if (!cond()) evalExpr(tail)
//          else False
//        case _ => evalExpr(tail)
//      }
//    }
//}
