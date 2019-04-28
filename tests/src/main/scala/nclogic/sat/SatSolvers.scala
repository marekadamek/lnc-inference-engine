package nclogic.sat

import bool._
import nclogic.model.expr.Expr

object SatSolvers {

  val miniSat: BoolSat = new BoolSat with MiniSat

  val glucose: BoolSat = new BoolSat with GlucoseSat

  val lingering: BoolSat = new BoolSat with LingeringSat

  val tableAux = new BoolSat {
    override def getSolution(e: Expr): Option[Set[Expr]] = TableAux.solveOne(e)

    override def getAllSolutions(e: Expr): Set[Set[Expr]] = TableAux.solveAll(e).toSet
  }

  val dpllLike = new BoolSat {
    override def getSolution(e: Expr): Option[Set[Expr]] = TableAuxBDD.solveOne(e)

    override def getAllSolutions(e: Expr): Set[Set[Expr]] = TableAuxBDD.solveAll(e)
  }
}
