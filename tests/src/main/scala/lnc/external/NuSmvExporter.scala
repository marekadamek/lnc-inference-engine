package lnc.external

import java.io.Writer

import lnc.expr._
import lnc.expr.converters.NormalFormConverter

import scala.collection.mutable

object NuSmvExporter extends Exporter {

  def convert(input: Expr, writer: Writer): Unit = {
    val ln = NormalFormConverter.convertToLN(input)
    val vars = mutable.Set.empty[Var]

    def loop(e: Expr): String = e match {
      case True => "True"
      case False => "False"
      case Var(x) =>
        vars += Var(x)
        x
      case Not(x) => "!" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, l) =>
        if (l == 0) loop(x)
        else "X(" + loop(Next(x, l - 1)) + ")"
    }

    val formula = "!G(" + loop(ln) + ")"

    val varDefs = vars.map(v => s"$v:boolean;").mkString(System.lineSeparator())

    writer.write(
      s"""
         |MODULE main
         |VAR
         |$varDefs
         |LTLSPEC
         |$formula
  """.stripMargin)
  }
}
