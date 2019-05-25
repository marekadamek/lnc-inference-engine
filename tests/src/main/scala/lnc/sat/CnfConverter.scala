package lnc.sat

import lnc.expr._
import lnc.sat.DnfConverter.convert

object CnfConverter {

  def convert(expr: Expr): Expr = expr match {
    // f((e1 & e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.find(_.isInstanceOf[And]) match {
      case None =>
        val converted = es map convert

        if (converted == es) Expr.or(es)
        else convert(Expr.or(converted))

      case Some(and) =>
        val rest = es - and
        val ors = and.asInstanceOf[And].es.map(e => Expr.or(rest + e))
        convert(Expr.and(ors))
    }
    // proceed conversion recursively
    case And(es) =>
      val converted = es map convert
      Expr.and(converted)

    case Impl(e1, e2) => convert(Expr.or(convert(Expr.not(e1)), convert(e2)))

    case Eq(e1, e2) =>
      val se1 = convert(e1)
      val se2 = convert(e2)

      convert(Expr.and(Expr.or(se1, Expr.not(se2)), Expr.or(Expr.not(se1), se2)))

    case e => e
  }
}
