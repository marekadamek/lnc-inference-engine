package lnc

import kripke.LNCToKripkeStructureConverter
import lnc.expr._
import lnc.mc.LNCModelChecker
import lnc.sat.SatSolvers
import time._

object LNCAsModel extends App with AppConfig {


  val a = Var("a")
  val formula = !a -> N(2, a)


  val (kripke, time) = measureTime {
    LNCToKripkeStructureConverter.convert(formula, SatSolvers.tableAux)
  }

  println(time.seconds)


  val contraExample = LNCModelChecker.verify(kripke, formula, SatSolvers.tableAux, None)

  println(contraExample)

}