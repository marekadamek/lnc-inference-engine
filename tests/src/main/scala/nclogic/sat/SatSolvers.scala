package nclogic.sat

import bool._

object SatSolvers {

  val miniSat: BoolSATBasedLNCSat = new BoolSATBasedLNCSat {
    protected val boolSat: BoolSat = new BoolSat with MiniSat {}
  }

  val glucose: BoolSATBasedLNCSat = new BoolSATBasedLNCSat {
    protected val boolSat: BoolSat = new BoolSat with GlucoseSat {}
  }

  val lingering: BoolSATBasedLNCSat = new BoolSATBasedLNCSat {
    protected val boolSat: BoolSat = new BoolSat with LingeringSat {}
  }

  val treengeling: BoolSATBasedLNCSat = new BoolSATBasedLNCSat {
    protected val boolSat: BoolSat = new BoolSat with TreengelingSat {}
  }

  val plingeling: BoolSATBasedLNCSat = new BoolSATBasedLNCSat {
    protected val boolSat: BoolSat = new BoolSat with PlingelingSat {}
  }
}
