package nclogic.sat

import nclogic.model.expr.{And, Expr, Or}

object CNFConverter {

  def convert(expr: Expr): Expr = expr.simplify match {
    // f((e1 & e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.find(_.isInstanceOf[And]) match {
      case None =>
        val converted = es map convert

        if (converted == es) Or(es)
        else convert(Or(converted))

      case Some(and) =>
        val rest = es - and
        val ors = and.asInstanceOf[And].es.map(e => Expr.or(rest + e))
        convert(Expr.and(ors))
    }
    // proceed conversion recursively
    case And(es) =>
      val converted = es map convert
      And(converted)
    case e => e
  }
}

