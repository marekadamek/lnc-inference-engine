package nclogic.model.expr

case class Term(value: Any) extends Unifiable {

  override def toString: String = value.toString

  def unify(exp: Unifiable, s: SubstitutionSet): SubstitutionSet = exp match {
    case x if x == this => s
    case Var(_) => exp.unify(this, s)
    case _ => null
  }

  override def replaceVariables(s: SubstitutionSet): Unifiable = this

  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}