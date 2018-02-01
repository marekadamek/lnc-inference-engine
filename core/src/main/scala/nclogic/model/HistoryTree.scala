package nclogic.model

import nclogic.model.converters.DnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat
import nclogic.tree.{MutableTree, TreeNode}


case class HistoryTree(protected val valuations: Expr) {

  private def toPath(e: Expr): List[Expr] = {
    def toPath(e: Expr, acc: List[Expr]): List[Expr] = {
      val currentTerms = e.getTerms.filterNot(t => t.isInstanceOf[TemporalExpr])
      val current = And(currentTerms).simplify

      getNext(e) match {
        case None => current :: acc
        case Some(next) =>
          //todo: handle cut
          val currentWithoutCut = And(next.getTerms.filterNot(_ == Cut))
          if (next == current) current :: acc
          else toPath(next, current :: acc)
      }


    }

    toPath(e, Nil).reverse
  }

  private def buildTree(paths: List[List[Expr]]): MutableTree[Expr] = {
    val root = new MutableTree[Expr](TreeNode(True), Nil)

    def addPathToTree(path: List[Expr], parent: MutableTree[Expr]): Unit = path match {
      case Nil => ()
      case p :: ps =>
        val childTree = parent.getOrElseAddAndGetChildTree(p)
        addPathToTree(ps, childTree)
    }

    paths.foreach(p => addPathToTree(p, root))
    root
  }

  val root: MutableTree[Expr] = valuations match {
    case Or(es) => buildTree(es.map(toPath))
    case e => buildTree(List(e).map(toPath))
  }

  private def getNext(clause: Expr): Option[Expr] = {
    val temporal = clause.getTerms.filter(_.isInstanceOf[TemporalExpr])
    val (dropTerms, nextTerms) = temporal.partition(_.isInstanceOf[Drop])
    val drop = dropTerms.map(_.asInstanceOf[Drop]).map(_.e)
    val next = nextTerms.map(_.asInstanceOf[Next]).map(_.e)

    //todo: CUT
//    val base = nonTemporal
//      .filterNot(e => next.contains(Neg(e).simplify))
//      .filterNot(_ == Cut)

    next.filterNot(drop.contains) match {
      case Nil => None
      case list => Some(And(list).simplify)
    }
  }

  def print() = root.print()
}