package nclogic.model.converters

import nclogic.model.{AndClause, Clause}
import nclogic.model.expr.{And, Expr, Or}
import nclogic._

object CnfConverter {
  def convert(expr: Expr): Expr = expr :> convertExpr

  private def convertExpr(expr: Expr): Expr = expr.simplify match {
    // f((e1 & e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.find(_.isInstanceOf[And]).map(_.asInstanceOf[And]) match {
      case Some(and) =>
        val rest = es - and
        val ors: Set[Expr] = and.es.map(e => Or(rest + e))
        convertExpr(And(ors))
      case _ =>
        val converted = es map convertExpr

        if (converted == es) Or(es)
        else convertExpr(Or(converted))
    }
    // proceed conversion recursively
    case And(es) =>
      val converted = es map convertExpr

      if (converted == es) And(es)
      else convertExpr(And(converted))
    case e => e
  }
}