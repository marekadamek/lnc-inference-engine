package lnc.sat.simulations

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.converters.NormalFormConverter._
import lnc.sat.{SatSolvers, TestFormulas}
import lnc.{AppConfig, LNC}
import time._

object Unsatisfiable_N_Formulas extends App with AppConfig {

  for {
    n <- Seq(10, 50, 100)
    d <- 100 to 1000 by 100
  } {

    val (formula, _) = {
      val (f, file) = TestFormulas.unsatisfiableN(n, d)
      val ln = convertToLN(f)
      val optimized = preprocess(ln)
      (optimized, file)
    }

    val (prefixFormula, prefixTime) = measureTime {
      LNC.calculatePrefixFormula(formula)
    }

    val (solution, tableAuxTime) = measureTime {
      prefixFormula match {
        case False => None
        case True => Some(True)
        case _ =>
          val normal = NormalFormConverter.convertToNormalForm(prefixFormula)
          SatSolvers.tableAux.iterator(normal).next()
      }
    }

    println(s"Test case: |AP|=$n, d=$d, sat=${solution.isDefined}")
    println(s"Prefix formula calculation time (s): ${prefixTime.seconds}")
    println(s"Tableaux time (s): ${tableAuxTime.seconds}")
    println()

  }
}