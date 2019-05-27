package lnc.sat

import java.nio.file.Paths

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.external.PltlExporter
import lnc.mc.{LNCFormulaGenerator, LNCModel}
import lnc.{AppConfig, LNC}
import time._

object RandomFormulas extends App with AppConfig {

  val n = 100
  val d = 5
  val size = 30
  val vars = (1 to n).map(i => Var(s"p$i")).toList

  def generateRandomFormulas(count: Int): List[Expr] = {
    1 to count map {_ => LNCFormulaGenerator.generateRandomFormula(vars, d, size) } toList
  }

  val formulas = generateRandomFormulas(100).map(NormalFormConverter.convertToLN)

  formulas.zipWithIndex.foreach { case (formula, idx) =>
    val pltlDir = config.getString("pltlFilesDir")
    PltlExporter.convert(formula, Paths.get(pltlDir, s"random_${n}_${d}_${size}_$idx.pltl"))

    //println(s"PLTL export time (ms): ${pltlTime.seconds}")
  }

  val times = formulas.zipWithIndex.map(f => {
    val optimized = NormalFormConverter.preprocess(f._1)

    val normal = NormalFormConverter.convertToNormalForm(optimized)

    val x = measureTime {
      LNCModel.findCycle(normal, SatSolvers.dpllLike).isDefined
    }

    println(f._2, x._1)

    x._2.seconds
  })

  println(times.sum)
}