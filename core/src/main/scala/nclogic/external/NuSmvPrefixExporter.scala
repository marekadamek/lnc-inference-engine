package nclogic.external

import java.io.Writer

import nclogic.model.NormalFormConverter
import nclogic.model.expr._

import scala.collection.mutable

object NuSmvPrefixExporter extends Exporter {

  def convert(input: Expr, writer: Writer): Unit = {
    val ln = NormalFormConverter.convertToLN(input)
    val vars = mutable.Set.empty[String]

    def loop(e: Expr): String = e match {
      case True => "True"
      case False => "False"
      case Var(x) =>
        vars += x
        x
      case Not(x) => "!" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, l) =>
        val v =   s"X_${l}_$x"
        vars += v
        v

    }

    val formula = "!(" + loop(ln) + ")"

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
