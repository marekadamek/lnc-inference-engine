package nclogic.external

import java.io.Writer

import nclogic.model.NormalFormConverter
import nclogic.model.expr._

object TrpExporter extends Exporter {

  def convert(input: Expr, writer: Writer): Unit = {
    val ln = NormalFormConverter.convertToLN(input)

    def loop(e: Expr): String = e match {
      case True => "True"
      case False => "False"
      case Var(x) => x
      case Not(x) => "~" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, l) =>
        if (l == 0) loop(x)
        else "next(" + loop(Next(x, l - 1)) + ")"
    }

    writer.write("always(" + loop(ln) + ")")
  }
}
