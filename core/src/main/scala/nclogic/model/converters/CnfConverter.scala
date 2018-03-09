package nclogic.model.converters

import nclogic.model.expr.{And, Expr, Or}

object CnfConverter {
  def convert(expr: Expr): Expr = convertExpr(expr.simplify)

  private def convertExpr(expr: Expr): Expr = expr.simplify match {
    // f((e1 & e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.indexWhere(_.isInstanceOf[And]) match {
      case -1 =>
        val converted = es map convertExpr

        if (converted.toSet == es.toSet) Or(es)
        else convertExpr(Or(converted))

      case idx =>
        val and = es(idx).asInstanceOf[And]
        val L = es.take(idx)
        val R = es.drop(idx + 1)
        val ors = and.es.map(e => Or(L ++ (e :: R)).simplify)
        convertExpr(And(ors))
    }
    // proceed conversion recursively
    case And(es) =>
      val converted = es map convertExpr
      And(converted).simplify
    case e => e
  }
}

