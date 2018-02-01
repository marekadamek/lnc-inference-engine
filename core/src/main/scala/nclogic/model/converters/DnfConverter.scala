package nclogic.model.converters

import nclogic._
import nclogic.model.converters.CnfConverter.convertExpr
import nclogic.model.expr.{And, Expr, Or}

object DnfConverter {
  def convert(expr: Expr): Expr = expr.simplify :> convertExpr

  private def convertExpr(expr: Expr): Expr = expr.simplify match {
    // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
    case And(es) => es.indexWhere(_.isInstanceOf[Or]) match {
      case -1 =>
        val converted = es map convertExpr

        if (converted == es) And(es)
        else convertExpr(And(converted))

      case idx =>
        val or = es(idx).asInstanceOf[Or]
        val L = es.take(idx)
        val R = es.drop(idx +1)
        val ands = or.es.map(e => And(L ++ (e :: R)))
        convertExpr(Or(ands))
    }
    // proceed conversion recursively
    case Or(es) =>
      val converted = es map convertExpr

      if (converted == es) Or(es)
      else convertExpr(Or(converted))
    case e => e
  }
}