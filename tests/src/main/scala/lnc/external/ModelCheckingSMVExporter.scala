package lnc.external

import java.io.{BufferedWriter, FileWriter, Writer}
import java.nio.file.Path

import lnc.AppConfig
import lnc.expr._
import lnc.expr.ltl.{Always, Finally, Release, Until}
import lnc.kripke.KripkeStructure

import scala.collection.mutable
import scala.util.Try

object ModelCheckingSMVExporter extends AppConfig {

  def convert(model: KripkeStructure, spec: List[Expr], path: Path): Unit = {
    val writer = new BufferedWriter(new FileWriter(path.toFile))
    val t = Try(convert(model: KripkeStructure, spec, writer))
    writer.close()
  }

  def convertSpec(spec: Expr): String = {
    def loop(e: Expr): String = e match {
      case True => "TRUE"
      case False => "FALSE"
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
      case Until(x, y) => "(" + loop(x) + " U " + loop(y) + ")"
      case Release(x, y) => "(" + loop(x) + " R " + loop(y) + ")"
    }

    loop(spec)
  }

  def convert(model: KripkeStructure, specs: List[Expr], writer: Writer): Unit = {

    val formulas = specs.map(x => convertSpec(x))

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
        val statesPos =
          if (pos.isEmpty) ""
          else pos.map(p => s"state = $p").mkString(" | ") + ": TRUE;"

        val statesNeg =
          if (neg.isEmpty) ""
          else neg.map(p => s"state = $p").mkString(" | ") + ": FALSE;"

        s"""
           |$v := case
           |  $statesPos
           |  $statesNeg
           |  TRUE: {TRUE, FALSE};
           | esac;
          """.stripMargin
      }
      .mkString("\n")

    val transitions = transitionMap
      .map { case (s, next) => s"state = $s : {${next.mkString(", ")}};" }
      .mkString("\n")

    val specBlock = formulas.map(f => s"LTLSPEC\n\t$f").mkString("\n")

    val vars = defineMap.keys.map(v => s"$v: boolean;").mkString("\n")

    writer.write(
      s"""
         |MODULE main
         |VAR
         |  state : {${states.mkString(", ")}};
         |  $vars
         |ASSIGN
         |   init(state) := {${states.mkString(", ")}};
         |   next(state) :=
         |    case
         |      $transitions
         |    esac;
         |    $definitions
         |$specBlock
  """.stripMargin)
  }
}
