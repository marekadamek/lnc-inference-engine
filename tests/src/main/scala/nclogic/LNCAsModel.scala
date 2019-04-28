package nclogic

import kripke.LNCToKripkeStructureConverter
import nclogic.mc.LNCMC
import nclogic.model.expr._
import nclogic.sat.SatSolvers
import time._

object LNCAsModel extends App with AppConfig {


  val a = Var("a")
  val formula = !a -> N(2, a)


  val (kripke, time) = measureTime {
    LNCToKripkeStructureConverter.convert(formula, SatSolvers.tableAux)
  }

  println(time.seconds)


  val contraExample = LNCMC.verify(kripke, formula, SatSolvers.tableAux)

  println(contraExample)

}