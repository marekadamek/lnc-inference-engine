package lnc.sat

import lnc.bool._
import lnc.expr.Expr
import lnc.expr.converters.NormalFormConverter

object SatSolvers {

  val miniSat: BoolSat = new BoolSat with MiniSat

  val glucose: BoolSat = new BoolSat with GlucoseSat

  val lingering: BoolSat = new BoolSat with LingeringSat

  val tableAux = new BoolSat {
    override def iterator(e: Expr): BoolSatIterator = TableAux(e)
  }

  val dpllLike = new BoolSat {

    override def iterator(e: Expr): BoolSatIterator = {
      val normal = NormalFormConverter.convertToNormalForm(e)
      DPLLLikeSatSolver(normal)
    }
  }
}
