package nclogic.model.expr

import nclogic.model.IdGenerator

case class Var(name: String) extends Unifiable {
  val id: Int = IdGenerator.next(Var.getClass)

  override def toString: String = name + "_" + id

  override def unify(exp: Unifiable, s: SubstitutionSet): SubstitutionSet = {
    if (exp == this) return s
    s.getBinding(this)
      .map(_.unify(exp, s))
      .getOrElse({
        val sNew = new SubstitutionSet(s)
        sNew.add(this, exp)
        sNew
      })

  }

  override def replaceVariables(s: SubstitutionSet): Expr =
    s.getBinding(this)
      .map(_.replaceVariables(s))
      .getOrElse(this)

  override def simplify: Expr = this

  override def isAtomic: Boolean = true
}