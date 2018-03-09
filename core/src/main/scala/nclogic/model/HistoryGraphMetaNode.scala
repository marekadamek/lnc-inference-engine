package nclogic.model

import nclogic.model.expr.Expr

case class HistoryGraphMetaNode(states: List[Expr]) {

  lazy val baseState: Expr = states.head
}
