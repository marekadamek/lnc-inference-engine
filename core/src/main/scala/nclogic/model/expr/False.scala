package nclogic.model.expr

case object False extends Expr {
  override val simplify: Expr = this

  override val toString: String = "FALSE"

  override def boolString: String = toString
}
