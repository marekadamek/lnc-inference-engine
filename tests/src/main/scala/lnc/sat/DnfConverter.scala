package lnc.sat

import lnc.expr._

object DnfConverter {

  def convert(expr: Expr): Expr = expr match {
    // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
    case And(es) => es.find(_.isInstanceOf[Or]) match {
      case None =>
        val converted = es map convert

        if (converted == es) Expr.and(es)
        else convert(Expr.and(converted))

      case Some(or) =>
        val rest = es - or
        val ands = or.asInstanceOf[Or].es.map(e => Expr.and(rest + e))
        convert(Expr.or(ands))
    }

    // proceed conversion recursively
    case Or(es) =>
      val converted = es map convert
      Expr.or(converted)

    case Impl(e1, e2) => convert(Expr.or(convert(Expr.not(e1)), convert(e2)))

    case Eq(e1, e2) =>
      val se1 = convert(e1)
      val se2 = convert(e2)

      convert(Expr.or(Expr.and(se1, se2), Expr.and(Expr.not(se1), Expr.not(se2))))

    case e => e
  }
}