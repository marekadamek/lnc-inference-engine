package lnc.mc

import java.nio.file.Paths

import lnc.AppConfig
import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.external.{ModelCheckingCTLSMVExporter, ModelCheckingLTLSPOTExporter}
import lnc.sat.SatSolvers
import time._

object KripkeRandom extends App with AppConfig {
  val n = 1
  val d = 0
  val size = 5

  val nodesCount = 1000

  val initialNodes = nodesCount
  val vars = (1 to n).map(i => Var(s"p$i")).toList


  private val targetDir =config.getString("nusmvMCFilesDir")
  private val spotTargetDir =config.getString("spotMCFilesTargetDir")

  val repeat = 100
  println("generating formulas")
  val specs = LNCFormulaGenerator.generateRandomFormulas(repeat, vars, d, size)
    .map(NormalFormConverter.convertToNormalForm)
  println("done")

  for (i <- 1 to 10) {
    val edgesCount = ((nodesCount * nodesCount) * 0.1 * i).toInt

    val model = KripkeStructureGenerator.generateRandomStructure(
      nodesCount = nodesCount,
      initialNodes = nodesCount,
      edgesCount = edgesCount,
      vars = vars
    )

    val filename = s"kripke_random${n}_${d}_${size}_$edgesCount"

    ModelCheckingLTLSPOTExporter.convert(model, specs, Paths.get(spotTargetDir, s"$filename.spot"))
    ModelCheckingCTLSMVExporter.convert(model, specs, Paths.get(targetDir, s"$filename.smv"))

    val (counterExamples, time) = measureTime {
      //LNCModelChecker.verify(model, specs, SatSolvers.tableAux, Some(100))
      LNCModelChecker.verify2(model, specs, SatSolvers.tableAux)
    }

        specs.zip(counterExamples)
          .filter(_._2.isEmpty)
          .foreach {
            case (spec, counterEx) =>
              println(spec)
              println(counterEx)
              println()
          }


    //println(List(edgesCount, time.seconds).mkString(","))
    println(List(edgesCount, time.seconds).mkString(","))

  }
}
