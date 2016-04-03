package nclogic.model

import nclogic._
import nclogic.model.Types.{And, Expr, Neg, Or}

object DnfConverter {
  type AndClause = Set[Expr]
  type DNF = Set[AndClause]

  def convert(expr: Expr): DNF = expr.simplify :> convertExpr :> makeSet :> simplify

  private def convertExpr(expr: Expr): Expr = {
    //println(expr)
    expr match {
      // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
      case And(Or(e1, e2), q) => Or(And(e1, q), And(e2, q)) :> convertExpr
      case And(q, Or(e1, e2)) => Or(And(q, e1), And(q, e2)) :> convertExpr

      case And(e1, e2) =>
        val converted = And(convertExpr(e1), convertExpr(e2))
        if (converted == expr) expr else convertExpr(converted)

      // proceed conversion recursively
      case Or(e1, e2) => Or(convertExpr(e1), convertExpr(e2))
      case _ => expr
    }
  }

  private def makeSet(expr: Expr): DNF = expr match {
    case Or(e1, e2) => makeSet(e1) ++ makeSet(e2)
    case And(e1, e2) => Set((makeSet(e1) ++ makeSet(e2)).flatten)
    case x: Expr => Set(Set(x))
  }

  def simplify(sets: DNF) = {
    sets.filterNot(s => s.exists(e1 => s.exists(e2 => e1 == Neg(e2).simplify)))
  }
}