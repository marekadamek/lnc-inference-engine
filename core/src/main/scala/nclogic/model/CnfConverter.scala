package nclogic.model

import nclogic.model.Types.{And, Expr, Neg, Or}


object CnfConverter {
  import nclogic._

  def convert(expr: Expr): Set[Set[Expr]] = expr :> convertExpr :> makeSet :> simplify

  def convertExpr(expr: Expr): Expr = expr match {
    // f((e1 | e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.find(_.isInstanceOf[And]) match {
      case Some(and@And(ands)) =>
        val rest = es.filterNot(_ == and)
        convertExpr(And(ands.map(a => Or(rest + a))).simplify)
      case _ => expr
    }
    // proceed conversion recursively
    case And(es) => And(es.map(convertExpr)).simplify
    case _ => expr
  }

  private def makeSet(expr: Expr): Set[Set[Expr]] = expr match {
    case And(es) => es flatMap makeSet
    case Or(es) => Set(es)
    case x: Expr => Set(Set(x))
  }

  def simplify(sets: Set[Set[Expr]]) = {
    sets.filterNot(s => s.exists(e1 => s.exists(e2 => e1 == Neg(e2).simplify)))
  }
}