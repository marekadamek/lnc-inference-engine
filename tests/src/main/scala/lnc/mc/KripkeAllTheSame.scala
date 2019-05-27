package lnc.mc

import java.nio.file.Paths

import lnc.AppConfig
import lnc.expr._
import lnc.external.{ModelCheckingSMVExporter, ModelCheckingSPOTExporter}
import lnc.sat.SatSolvers
import time._

object KripkeAllTheSame extends App with AppConfig {

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  val p1 = Var("p1")

  val nodesCount = 1000
  val initialNodes = 1000
  val edgesCount = 50000

  val model = KripkeStructureGenerator.allNodesTheSame(
    nodesCount = nodesCount,
    initialNodes = initialNodes,
    edgesCount = edgesCount,
    terms = List(p1)
  )

  for (d <- 0 to 200 by 20) {
    val spec = Expr.and((0 to d).map(i => N(i, p1)).toSet)

    ModelCheckingSMVExporter.convert(model, List(spec), Paths.get(targetDir, s"kripke_all_$d.smv"))
    ModelCheckingSPOTExporter.convert(model, List(spec), Paths.get(spotTargetDir, s"kripke_all_$d.spot"))

    val (result, time) = measureTime {
      LNCModelChecker.verify2(model, List(spec), SatSolvers.tableAux)
    }

    println(List(d, time.seconds).mkString(","))
  }
}