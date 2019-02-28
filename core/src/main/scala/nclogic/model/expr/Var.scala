package nclogic.model.expr

case class Var(name: String) extends Expr {

  override val toString: String = name.toString

  override val simplify: Expr = this

  override def boolString: String = toString

}