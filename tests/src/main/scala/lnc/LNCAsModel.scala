package lnc

import java.nio.file.Paths

import lnc.expr._
import lnc.expr.converters.PrefixFormulaConverter
import lnc.external.{ModelCheckingCTLSMVExporter, ModelCheckingLTLSPOTExporter}
import lnc.kripke.LNCToKripkeStructureConverter
import lnc.mc.{LNCFormulaGenerator, LNCModelChecker}
import lnc.sat.SatSolvers
import time._

import scala.util.Random

object LNCAsModel extends App with AppConfig {

  val rnd = new Random

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  val count = 100
  val times = (1 to count).map(i => {
    println(i)
    val n = 10 + rnd.nextInt(20)
    val d = 1 + rnd.nextInt(5)
    val size = 5 + rnd.nextInt(15)
    val vars = (1 to n).map(i => Var(s"p$i")).toList

    println(i, n, d, size)
    println("generation...")
    val formula = LNCFormulaGenerator.generateRandomFormula(vars, d, size)

    println("conversion...")
    val (kripke, time) = measureTime {
      LNCToKripkeStructureConverter.convert(formula, SatSolvers.dpllLike)
    }

    val allSolutions = SatSolvers.dpllLike.getAllSolutions(PrefixFormulaConverter.prefixFormula(formula)).map(Expr.and).toList

    println()
    ModelCheckingLTLSPOTExporter.convert(kripke, List(formula), Paths.get(spotTargetDir, s"lncmodel/$i.spot"))
    ModelCheckingCTLSMVExporter.convert(kripke, allSolutions, Paths.get(targetDir, s"lncmodel/$i.smv"))

    time.seconds
  })

  println(times.sum)

}

object Test extends App {
  val a = Expr.v("a")
  val k = LNCToKripkeStructureConverter.convert(a | N(2,a), SatSolvers.dpllLike)
  k.prettyPrint()
}