package lnc.expr

/**
  * Represents temporal operator N in LNC logic (it's equivalent to operator X in LTL)
  * @param e operand
  * @param level used for shorter notation of nested N operators. Next(p, 3) is corresponds to N(N(Np))
  */
case class Next(e: Expr, level: Int) extends Expr {
  override lazy val toString: String = {
    val d = if (level > 1) s"^$level" else ""
    s"N$d($e)"
  }
}
