package lnc.external

import java.io.Writer

import lnc.expr._
import lnc.expr.ltl.{Always, Finally, Release, Until}

import scala.collection.mutable.ListBuffer

object PltlExporter extends Exporter {

  def convertOne(input: Expr, writer: Writer): Unit = {
    var elems = List[Any](input)

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

        case Always(x) =>
          List("G(", x, ")")

        case Finally(x) =>
          List("F(", x, ")")

        case Until(x, x2) =>
          List("(", x, "U", x2, ")")

        case Release(x, x2) =>
          List("(", x, "R", x2, ")")
      }

      elems = newElems ::: tail
    }
  }

  def convert(input: List[Expr], writer: Writer): Unit = {
    input.foreach { e =>
      convertOne(e, writer)
      writer.write("\n")
    }
  }
}
