package nclogic.model.expr

case class Block(name: String, b: () => Unit) extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  def execute(): Unit = b()

  override def toString = name

  override def replaceVariables(s: SubstitutionSet): Expr = this

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}
