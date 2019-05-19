package lnc.expr

/**
  * Atomic preposition
  * @param name - atomic preposition name
  */
case class Var(name: String) extends Expr {
  override val toString: String = name.toString
}