package lnc.external

import lnc.expr._

import scala.collection.mutable.ListBuffer

object BoolExporter {

  def convert(input: Expr): String = {
    val sb = new StringBuilder
    var elems = List[Any](input)

    while (elems.nonEmpty) {
      val (e, tail) = (elems.head, elems.tail)

      val newElems = e match {
        case Var(x) =>
          sb.append(x)
          Nil

        case Not(x) =>
          List("!", x)

        case And(es) =>
          val lb = new ListBuffer[Any]
          val (e1, rest) = {
            val l = es.toList
            (l.head, l.tail)
          }

          lb.append("(", e1)
          rest.foreach(e => lb.append(" & ", e))
          lb.append(")")

          lb.toList
        case Or(es) =>
          val lb = new ListBuffer[Any]
          val (e1, rest) = {
            val l = es.toList
            (l.head, l.tail)
          }

          lb.append("(", e1)
          rest.foreach(e => lb.append(" | ", e))
          lb.append(")")

          lb.toList
        case Impl(e1, e2) =>
           List("(", e1, " -> ", e2, ")")

        case Eq(e1, e2) =>
          List("(", e1, " = ", e2, ")")

        case Next(x, l) =>
          if (l == 0)  List(x)
          else List(s"X_${l}_", x)

        case _ =>
          sb.append(e)
          Nil
      }

      elems = newElems ::: tail
    }

    sb.toString()
  }
}
