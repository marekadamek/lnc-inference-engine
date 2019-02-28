package nclogic.external

import java.io.Writer

import nclogic.model.NormalFormConverter
import nclogic.model.expr._

object AaltaExporter extends Exporter {

  def convert(input: Expr, writer: Writer): Unit = {
    val ln = NormalFormConverter.convertToLN(input)

    def loop(e: Expr): String = e match {
      case True => "true"
      case False => "false"
      case Var(x) => x
      case Not(x) => "~" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, l) =>
        if (l == 0) loop(x)
        else "X(" + loop(Next(x, l - 1)) + ")"
    }

    writer.write("G(" + loop(ln) + ")")
  }
}
