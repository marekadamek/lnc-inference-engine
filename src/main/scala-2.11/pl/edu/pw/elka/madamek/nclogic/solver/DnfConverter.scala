package pl.edu.pw.elka.madamek.nclogic.solver

import pl.edu.pw.elka.madamek.nclogic.model.Types._

trait DnfConverter {
  def convert(expr: Expr): Set[Set[Expr]]
}

object DnfConverter extends DnfConverter {

  def convert(expr: Expr): Set[Set[Expr]] = (convertExpr _ andThen makeSet andThen simplify)(expr)

  def convertExpr(expr: Expr): Expr = expr match {
    // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
    case And(es) => es.find(_.isInstanceOf[Or]) match {
      case Some(or@Or(ors)) =>
        val rest = es - or
        Or(ors.map(a => convertExpr(And(rest + a)))).simplify
      case _ => expr
    }
    // proceed conversion recursively
    case Or(es) => Or(es.map(convertExpr)).simplify
    case _ => expr
  }

  private def makeSet(expr: Expr): Set[Set[Expr]] = expr match {
    case Or(es) => es flatMap makeSet
    case And(es) => Set(es)
    case x: Expr => Set(Set(x))
  }

  def simplify(sets: Set[Set[Expr]]) = {
    sets.filterNot(s => s.exists(e1 => s.exists(e2 => e1 == Neg(e2).simplify)))
  }
}