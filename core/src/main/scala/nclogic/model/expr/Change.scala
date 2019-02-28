package nclogic.model.expr

case class Change(e: Expr, level: Int) extends Expr {
  override lazy val toString = {
    val d = if (level > 1) s"^$level" else ""
    s"C$d($e)"
  }

  override lazy val boolString: String = ???



  lazy val simplify: Expr = e match {
    case Not(x) => Change(x, level).simplify
    case _ =>
      val s = if (level == 1) e.simplify else Change(e, level - 1).simplify
      ((s & !N(s)) | (!s & N(s))).simplify
  }
}
