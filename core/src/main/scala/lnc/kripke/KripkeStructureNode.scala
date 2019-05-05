package lnc.kripke

import lnc.expr._

case class KripkeStructureNode(id: Int, terms: Set[Expr], initial: Boolean) {
  require(terms.forall {
    case True | Var(_) | Not(Var(_)) => true
    case _ => false
  }, "Kripke structure node can only contain literals or True")
}
