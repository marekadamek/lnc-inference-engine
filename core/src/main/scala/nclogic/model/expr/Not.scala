package nclogic.model.expr

case class Not(e: Expr) extends Expr {
  override lazy val toString = s"!$e"

  lazy val simplify: Expr = e match {
      case True => False
      case False => True
      case Var(_) => this
      case Next(Var(x), d) => Not(Next(Var(x), d).simplify)
      case Not(x) => x.simplify
      case And(es) => Or(es.map(Not.apply).map(_.asInstanceOf[Expr])).simplify
      case Or(es) => And(es.map(Not.apply).map(_.asInstanceOf[Expr])).simplify
      case _ => Not(e.simplify).simplify
    }

  override def boolString: String = s"!${e.boolString}"
}
