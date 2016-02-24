package pl.edu.pw.elka.madamek.nclogic.solver

import pl.edu.pw.elka.madamek.nclogic.model.Types._

trait CnfConverter {
  def convert(expr: Expr): Expr
}

object CnfConverter extends CnfConverter {

  def convert(expr: Expr): Expr = expr match {
    // f((e1 | e2) | e) -> f(e1 | e) & f(e2 | e)
    case Or(es) => es.find(_.isInstanceOf[And]) match {
      case Some(and@And(ands)) =>
        val rest = es.filterNot(_ == and)
        And(ands.map(a => convert(Or(rest + a)))).simplify
      case _ => expr
    }
    // proceed conversion recursively
    case And(es) => And(es.map(convert)).simplify
    case _ => expr
  }
}