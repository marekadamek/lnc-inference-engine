package nclogic.model.expr

trait Unifiable extends Expr {
  def unify(exp: Unifiable, s: SubstitutionSet): SubstitutionSet
}
