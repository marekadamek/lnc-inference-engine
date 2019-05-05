package lnc.expr

case class Var(name: String) extends Expr {
  override val toString: String = name.toString
}