package lnc.expr

/**
  * Represents temporal operator C in LNC logic
  * @param e operand
  * @param level used for shorter notation of nested C operators. Change(p, 3) is corresponds to C(C(Cp))
  */
case class Change(e: Expr, level: Int) extends Expr {
  override lazy val toString = {
    val d = if (level > 1) s"^$level" else ""
    s"C$d($e)"
  }
}
