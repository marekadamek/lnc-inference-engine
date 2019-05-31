package lnc.external

import java.io.{BufferedWriter, FileWriter, Writer}
import java.nio.file.Path

import lnc.AppConfig
import lnc.expr.ltl.{Always, Finally, Release, Until}
import lnc.expr.{Expr, _}
import lnc.kripke.KripkeStructure

import scala.collection.mutable
import scala.util.Try

object ModelCheckingLTLSPOTExporter extends AppConfig {

  def convert(model: KripkeStructure, specs: List[Expr], path: Path): Unit = {
    val writer = new BufferedWriter(new FileWriter(path.toFile))
    val t = Try(convert(model, specs, writer))
    writer.close()
  }

  def convertSpec(spec: Expr, op: String): String = {
    def loop(e: Expr): String = e match {
      case True => "True"
      case False => "False"
      case Var(x) => x
      case Not(x) => "!" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, 1) => s"X(${loop(x)})"
      case Next(x, l) => s"X(${loop(Next(x, l - 1))})"
      case Always(x) => s"G(${loop(x)})"
      case Finally(x) => s"F(${loop(x)})"
      case Until(x1, x2) => s"(${loop(x1)} U ${loop(x2)})"
      case Release(x1, x2) => s"(${loop(x1)} R ${loop(x2)})"
    }

    s"${loop(spec)}"
  }

  def convert(model: KripkeStructure, spec: List[Expr], writer: Writer): Unit = {
    val states = mutable.Set.empty[(String, String, Boolean)]
    val ap = mutable.Set.empty[String]
    val edges = mutable.Set.empty[String]

    model.getNodes.foreach(node => {
      val state = s"s${node.id}"


      val terms = node.terms.filter {
        case Var(_) | Not(Var(_)) => true
        case _ => false
      }

      val nodeAp = terms.map {
        case Var(v) => v
        case Not(Var(v)) => v
      }

      ap ++= nodeAp

      val formula = terms.map {
        case Var(v) => v
        case Not(Var(v)) => "-" + v
      } mkString " "

      states += ((state, formula, node.initial))

      edges ++= {
        model.getSuccessors(node.id)
          .map(s => s"s${node.id} s${s.id}")
      }
    })


    val statesDescription = states
      .map {
        case (id, body, initial) =>
          val init = if (initial) "1" else ""
          s"$id $init\n$body"
      }
      .mkString("\n")

    val specsCount = spec.size
    writer.write(
      s"""|${ap.size} ${states.size} ${edges.size} $specsCount
          |${ap.mkString("\n")}
          |$statesDescription
          |${edges.mkString("\n")}
          |$specsCount
          |${spec.map(convertSpec(_, "G")).mkString("\n")}
          |""".stripMargin)
  }

}