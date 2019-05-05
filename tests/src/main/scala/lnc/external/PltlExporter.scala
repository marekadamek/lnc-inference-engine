package lnc.external

import java.io.Writer

import lnc.expr.{_}

import scala.collection.mutable.ListBuffer

object PltlExporter extends Exporter {

  def convert(input: Expr, writer: Writer): Unit = {
    var elems = List[Any](input)

   // writer.write("G(")

    while (elems.nonEmpty) {
      val (e, tail) = (elems.head, elems.tail)

      val newElems = e match {
        case x: String =>
          writer.write(x)
          Nil

        case True =>
          writer.write("True")
          Nil
        case False =>
          writer.write("False")
          Nil
        case Var(x) =>
          writer.write(x)
          Nil

        case Not(x) =>
          writer.write("~")
          List(x)

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
          List("(", e1, " => ", e2, ")")

        case Eq(e1, e2) =>
          List("(", e1, " <=> ", e2, ")")

        case Next(x, l) =>
          if (l == 0) List(x)
          else List("X(", Next(x, l - 1), ")")
      }

      elems = newElems ::: tail
    }

   // writer.write(")")
  }
}
