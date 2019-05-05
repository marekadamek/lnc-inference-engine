package lnc

import lnc.expr._
import lnc.expr.converters.PrefixFormulaConverter
import lnc.expr.ltl.{Finally, LTLOperator}
import lnc.sat.TableAux

object LNCByLTL extends App with AppConfig {


  val a = Var("a")
  val formula = !a -> N(2, a)

  val lnc = PrefixFormulaConverter.prefixFormula(formula)
  val spec = !a -> Finally(a)

  def verify(model: Expr, spec: Expr) = {
    var modelD = LNC.depth(model)
    val d = Math.max(modelD, LNC.depth(spec))

    var effectiveModel = model
    while (modelD < d) {
      effectiveModel = Expr.and(effectiveModel, N(effectiveModel))
      modelD += 1
    }


    val specInLNC = LTLOperator.toLnc(spec, d)

    val effectiveFormula = effectiveModel & !specInLNC
    val contra = TableAux(effectiveFormula).next()
    println(contra)
  }

  verify(formula, spec)

}