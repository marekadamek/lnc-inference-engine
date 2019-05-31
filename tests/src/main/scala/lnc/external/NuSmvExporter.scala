package lnc.external

import java.io.Writer

import lnc.expr._
import lnc.expr.converters.NormalFormConverter

import scala.collection.mutable

object NuSmvExporter extends Exporter {

  def convert(input: List[Expr], writer: Writer): Unit = {
    val vars = mutable.Set.empty[Var]

    def loop(e: Expr): String = e match {
      case True => "TRUE"
      case False => "FALSE"
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

    val specs = input
      .map(f => {
        val ln = NormalFormConverter.convertToLN(f)
        "LTLSPEC\n!G(" + loop(ln) + ")"
      })
      .mkString("\n")

    val varDefs = vars.map(v => s"$v:boolean;").mkString(System.lineSeparator())

    writer.write(
      s"""
         |MODULE main
         |VAR
         |$varDefs
         |$specs
  """.stripMargin)
  }
}
