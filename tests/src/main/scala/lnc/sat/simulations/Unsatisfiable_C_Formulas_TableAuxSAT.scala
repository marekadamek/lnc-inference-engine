package lnc.sat.simulations

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.converters.NormalFormConverter._
import lnc.sat.{SatSolvers, TestFormulas}
import lnc.{AppConfig, LNC}
import time._

object Unsatisfiable_C_Formulas_TableAuxSAT extends App with AppConfig {

  for {
    n <- Seq(1, 10, 50)
    d <- 1 to 16
  } {

    val (formula, _) = TestFormulas.unsatisfiableC(n, d)

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