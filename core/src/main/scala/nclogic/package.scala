import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr.{Expr, False, Neg}
import nclogic.sat.Sat

package object nclogic {

  def isTautology(formula: Expr): Boolean = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr): Boolean = Sat.solve(CnfConverter.convert(formula)) == False
}
