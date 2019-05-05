package lnc.external

import java.io.{BufferedWriter, FileWriter, Writer}
import java.nio.file.Path

import lnc.kripke.KripkeStructure
import lnc.AppConfig
import lnc.expr._
import lnc.expr.converters.NormalFormConverter

import scala.collection.mutable
import scala.util.Try

object ModelCheckingSMVExporter extends AppConfig {

  def convert(model: KripkeStructure, spec: Expr, path: Path): Unit = {
    val writer = new BufferedWriter(new FileWriter(path.toFile))
    val t = Try(convert(model: KripkeStructure, spec: Expr, writer))
    writer.close()
  }

  def convertSpec(spec: Expr): String = {
    def loop(e: Expr): String = e match {
      case True => "True"
      case False => "False"
      case Var(x) => x
      case Not(x) => "!" + loop(x)
      case And(es) => "(" + es.map(loop).mkString(" & ") + ")"
      case Or(es) => "(" + es.map(loop).mkString(" | ") + ")"
      case Impl(e1, e2) => "(" + loop(e1) + " -> " + loop(e2) + ")"
      case Eq(e1, e2) => "(" + loop(e1) + " <-> " + loop(e2) + ")"
      case Next(x, 1) =>
        val v = s"AX(${loop(x)})"
        // vars += v
        v

      case Next(x, l) =>
        val v = s"AX(${loop(Next(x, l-1))})"
        // vars += v
        v
    }

    loop(spec)
  }

  def convert(model: KripkeStructure, spec: Expr, writer: Writer): Unit = {
    val lnSpec = NormalFormConverter.convertToLN(spec)
    val formula = "AG(" + convertSpec(lnSpec) + ")"

    val states = mutable.Set.empty[String]
    val initialStates = mutable.Set.empty[String]
    val defineMap = mutable.Map.empty[Var, (Set[String], Set[String])]
    val transitionMap = mutable.Map.empty[String, Set[String]]

    model.getNodes.foreach(node => {
      val state = s"s${node.id}"
      states += state
      if (node.initial) {
        initialStates += state
      }

      node.terms.foreach {
        case v: Var =>
          val (pos, neg) = defineMap.getOrElse(v, (Set.empty[String], Set.empty[String]))
          defineMap.put(v, (pos + state, neg))
        case Not(Var(v)) =>
          val (pos, neg) = defineMap.getOrElse(Var(v), (Set.empty[String], Set.empty[String]))
          defineMap.put(Var(v), (pos, neg + state))
        case True =>
      }

      val next = model.getSuccessors(node.id).map(s => s"s${s.id}")
      transitionMap.put(state, next)
    })

    val definitions = defineMap
      .map { case (Var(v), (pos, neg)) =>
        val states = (pos.map(p => s"state = $p") ++ neg.map(n => s"state != $n")).mkString(" | ")
        s"$v := $states;"
      }
      .mkString("\n")

    val transitions = transitionMap
      .map { case (s, next) => s"state = $s : {${next.mkString(", ")}};" }
      .mkString("\n")

    writer.write(
      s"""
         |MODULE main
         |VAR
         |  state : {${states.mkString(", ")}};
         |ASSIGN
         |   init(state) := {${states.mkString(", ")}};
         |   next(state) :=
         |    case
         |      $transitions
         |    esac;
         |DEFINE
         |  $definitions
         |SPEC
         |  $formula;
  """.stripMargin)
  }
}
