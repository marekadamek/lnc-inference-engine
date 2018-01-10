package nclogic.model.converters

import nclogic._
import nclogic.model.expr.{And, Expr, Or}

object DnfConverter {
  def convert(expr: Expr): Expr = expr.simplify :> convertExpr

  private def convertExpr(expr: Expr): Expr = expr.simplify match {
    // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
    case And(es) => es.find(_.isInstanceOf[Or]).map(_.asInstanceOf[Or]) match {
      case Some(or) =>
        val rest = es - or
        Or(or.es.map(e => And(rest + e))) :> convertExpr
      case _ =>
        val converted = es map convertExpr

        if (converted == es) And(es)
        else convertExpr(And(converted))
    }
    // proceed conversion recursively
    case Or(es) =>
      val converted = es map convertExpr

      if (converted == es) Or(es)
      else convertExpr(Or(converted))
    case e => e
  }
}