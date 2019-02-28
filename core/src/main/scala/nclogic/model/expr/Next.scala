package nclogic.model.expr

case class Next(e: Expr, level: Int) extends Expr {
  override lazy val toString: String = {
    val d = if (level > 1) s"^$level" else ""
    s"N$d($e)"
  }

  def ^(l: Int) = Next(e, level + l)

  /**
    * N(a | b) <=> N(a) | N(b)
    * N(a & b) <=> N(a) & N(b)
    * N(a => b) <=> N(a) => N(b)
    * N(a <=> b) <=> N(a) <=> N(b)
    * N(!a) <=> !N(a) (implementation in Neg)
    */
  lazy val simplify: Expr = {
    if (level == 0) e.simplify
    else {
      e match {
        case Var(_) => this
        case Not(x) => Not(Next(x, level)).simplify
        case Next(x, l) => Next(x, l + 1).simplify
        case Or(es) => Or(es.map(Next(_, level)).map(_.asInstanceOf[Expr])).simplify
        case And(es) => And(es.map(Next(_, level)).map(_.asInstanceOf[Expr])).simplify
        case Impl(p, q) => Impl(Next(p, level), Next(q, level)).simplify
        case Eq(p, q) => Eq(Next(p,level), Next(q,level)).simplify
        case _ => Next(e.simplify,level).simplify
      }
    }
  }

  override def boolString: String = {
    s"X_${level}_${e.boolString}"
  }
}
