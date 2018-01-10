package nclogic.model

import nclogic.model.expr.Expr

trait Clause {
  def terms: Set[Expr]
}

case object EmptyClause extends Clause {
  override def terms: Set[Expr] = Set.empty
}
case class AndClause(es: Set[Expr]) extends Clause {
  require(es forall {_.isAtomic}, "all terms is clause has to be atomic")

  override def terms: Set[Expr] = es
}