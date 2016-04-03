package nclogic.model

import nclogic._
import nclogic.model.Types.{And, Expr, Or}

object CnfConverter {
  type OrClause = Set[Expr]
  type CNF = Set[OrClause]

  def convert(expr: Expr): CNF = expr.simplify :> convertExpr :> makeSet

  private def convertExpr(expr: Expr): Expr = {
    //println(expr)
    expr match {
      // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
      case Or(And(e1, e2), q) => And(Or(e1, q), Or(e2, q)) :> convertExpr
      case Or(q, And(e1, e2)) => And(Or(q, e1), Or(q, e2)) :> convertExpr

      case Or(e1, e2) =>
        val converted = Or(convertExpr(e1), convertExpr(e2)).simplify
        if (converted == expr) expr else convertExpr(converted)

      // proceed conversion recursively
      case And(e1, e2) => And(convertExpr(e1), convertExpr(e2)).simplify
      case _ => expr
    }
  }

  private def makeSet(expr: Expr): CNF = expr match {
    case And(e1, e2) => makeSet(e1) ++ makeSet(e2)
    case Or(e1, e2) => Set((makeSet(e1) ++ makeSet(e2)).flatten)
    case x: Expr => Set(Set(x))
  }

}