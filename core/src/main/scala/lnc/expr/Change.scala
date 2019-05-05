package lnc.expr

case class Change(e: Expr, level: Int) extends Expr {
  override lazy val toString = {
    val d = if (level > 1) s"^$level" else ""
    s"C$d($e)"
  }
}
