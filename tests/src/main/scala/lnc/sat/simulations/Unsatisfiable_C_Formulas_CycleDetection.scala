package lnc.sat.simulations

import lnc.AppConfig
import lnc.expr.converters.NormalFormConverter._
import lnc.mc.LNCModel
import lnc.sat.{SatSolvers, TestFormulas}
import time._

object Unsatisfiable_C_Formulas_CycleDetection extends App with AppConfig {

  for {
    n <- Seq(1, 10, 50)
    d <- 1 to 16
  } {
    val (formula, _) = {
      val (f, file) = TestFormulas.unsatisfiableC(n, d)
      val ln = convertToLN(f)
      val optimized = preprocess(ln)
      (optimized, file)
    }

    val (cycle, cycleTIme) = measureTime {
      LNCModel.findCycle(formula, SatSolvers.tableAux)
    }

    println(s"Test case: |AP|=$n, d=$d, sat=${cycle.isDefined}")
    println(s"Cycle detection time (s): ${cycleTIme.seconds}")
    println()

  }
}