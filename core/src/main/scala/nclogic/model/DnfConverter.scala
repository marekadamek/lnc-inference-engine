package nclogic.model

import nclogic._
import nclogic.model.Types.{And, Expr, Neg, Or}

object DnfConverter {
  type AndClause = Set[Expr]
  type DNF = Set[AndClause]

  def convert(expr: Expr): DNF = expr.simplify :> convertExpr :> makeSet :> simplify

  private def convertExpr(expr: Expr): Expr = {
    println(expr)
    expr match {
      // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
      case And(es) => es.find(_.isInstanceOf[Or]) match {
        case Some(or@Or(ors)) =>
          val rest = es - or
          convertExpr(Or(ors.map(a => And(rest + a))).simplify)
        case _ => expr
      }
      // proceed conversion recursively
      case Or(es) => Or(es.map(convertExpr)).simplify
      case _ => expr
    }
  }

  private def makeSet(expr: Expr): DNF = expr match {
    case Or(es) => es flatMap makeSet
    case And(es) => Set(es)
    case x: Expr => Set(Set(x))
  }

  def simplify(sets: DNF) = {
    sets.filterNot(s => s.exists(e1 => s.exists(e2 => e1 == Neg(e2).simplify)))
  }
}