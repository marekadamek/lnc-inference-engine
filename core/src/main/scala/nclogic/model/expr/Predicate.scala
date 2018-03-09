package nclogic.model.expr

case class Predicate(name: String, args: List[Unifiable]) extends Unifiable {

  override def toString: String = name + "(" + args.map(_.toString).mkString(" ") + ")"

  override def unify(exp: Unifiable, s: SubstitutionSet): SubstitutionSet = exp match {
    case Predicate(_, oTerms) =>
      if (args.lengthCompare(oTerms.length) != 0) {
        return null
      }

      var sNew = new SubstitutionSet(s)
      for (i <- args.indices) {
        sNew = args(i).unify(oTerms(i), sNew)
        if (sNew == null) {
          return null
        }
      }
      sNew

    case Var(_) => exp.unify(this, s)

    case _ => null
  }

  def replaceVariables(s: SubstitutionSet): Expr = {
    val newArgs = args.map(_.replaceVariables(s).asInstanceOf[Unifiable])
    Predicate(name, newArgs)
  }

  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}
