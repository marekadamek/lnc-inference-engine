package lnc.expr

case class Next(e: Expr, level: Int) extends Expr {
  override lazy val toString: String = {
    val d = if (level > 1) s"^$level" else ""
    s"N$d($e)"
  }
}
