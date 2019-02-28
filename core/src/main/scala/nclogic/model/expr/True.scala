package nclogic.model.expr

case object True extends Expr {
  override val simplify: Expr = this

  override val toString: String = "TRUE"

  override def boolString: String = toString
}
