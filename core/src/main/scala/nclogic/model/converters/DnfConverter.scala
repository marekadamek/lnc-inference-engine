package nclogic.model.converters

import nclogic.java.model.expr.{And, Expr, Or}

object DnfConverter {

  def convert(expr: Expr): Expr = expr.simplify match {
    // f((e1 | e2) & e) -> f(e1 & e) | f(e2 & e)
    case And(es) => es.find(_.isInstanceOf[Or]) match {
      case None =>
        val converted = es map convert

        if (converted == es) And(es).simplify
        else convert(And(converted))

      case Some(or) =>
        val rest = es - or
        val ands = or.asInstanceOf[Or].es.map(e => And(rest + e).simplify)
        convert(Or(ands))
    }
    // proceed conversion recursively
    case Or(es) =>
      val converted = es map convert
      Or(converted).simplify
    case e => e
  }
}