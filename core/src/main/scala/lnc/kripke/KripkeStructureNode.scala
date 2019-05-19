package lnc.kripke

import lnc.expr._

/**
  * Represents Kripke structure node
  * @param id id
  * @param terms set of terms true in this state
  * @param initial true if this state is initial, otherwise false
  */
case class KripkeStructureNode(id: Int, terms: Set[Expr], initial: Boolean) {
  require(terms.forall {
    case True | Var(_) | Not(Var(_)) => true
    case _ => false
  }, "Kripke structure node can only contain literals or True")
}
