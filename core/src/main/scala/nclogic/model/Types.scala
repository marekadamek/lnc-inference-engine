package nclogic.model

object Types {

  sealed trait Expr {
    def simplify: Expr
  }

  case class And(e1: Expr, e2: Expr) extends Expr {
    override def toString = {
      val result = s"${es.map(_.toString).mkString(" & ")}"
      if (es.size == 1) result else s"($result)"
    }

    def simplify: Expr = es match {
      case x if x.contains(Const(false)) => Const(false)
      case x if x.exists(e1 => x.exists(e2 => e1 == Neg(e2) || e2 == Neg(e1))) => Const(false)
      case _ =>
        val simplified = es
          .flatMap {
            case And(x) => x
            case x => Set(x)
          }
          .map {
            _.simplify
          }

        if (es == simplified)
          this
        else
          And(simplified).simplify
    }
  }

  case class Or(es: Set[Expr]) extends Expr {
    override def toString = {
      val result = s"${es.map(_.toString).mkString(" | ")}"
      if (es.size == 1) result else s"($result)"
    }

    def simplify: Expr = es match {
      case x if x.contains(Const(true)) => Const(true)
      case x if x.exists(e1 => x.exists(e2 => e1 == Neg(e2) || e2 == Neg(e1))) => Const(true)
      case _ =>
        val simplified = es
          .flatMap {
            case Or(x) => x
            case x => Set(x)
          }
          .map {
            _.simplify
          }

        if (es == simplified)
          this
        else
          Or(simplified).simplify
    }
  }

  case class Impl(t1: Expr, t2: Expr) extends Expr {
    override def toString = s"${t1.toString} => ${t2.toString}"

    def simplify = Or(Set(Neg(t1), t2)).simplify
  }

  case class Eq(e1: Expr, e2: Expr) extends Expr {
    override def toString = s"$e1 <=> $e2"

    def simplify = And(Set(Impl(e1, e2), Impl(e2, e1))).simplify

    override def equals(any: Any) = any match {
      case o: Eq => Set(e1, e2) == Set(o.e1, o.e2)
      case _ => false
    }
  }

  case class C(e: Expr) extends Expr {
    override def toString = s"C($e)"

    def simplify = Eq(e, Neg(N(e))).simplify

  }

  case class N(e: Expr) extends Expr {
    override def toString = s"N($e)"

    /**
     * N(a | b) <=> N(a) | N(b)
     * N(a & b) <=> N(a) & N(b)
     * N(a => b) <=> N(a) => N(b)
     * N(a <=> b) <=> N(a) <=> N(b)
     */
    def simplify: Expr = e match {
      case Or(es) => Or(es map N).simplify
      case And(es) => And(es map N).simplify
      case Impl(p, q) => Impl(N(p), N(q)).simplify
      case Eq(p, q) => Eq(N(p), N(q)).simplify
      case _ => N(e.simplify)
    }
  }

  case class Neg(e: Expr) extends Expr {
    override def toString = "!" + e

    def simplify: Expr = e match {
      case Const(b) => Const(!b)
      case Var(_) => this
      case Neg(x) => x.simplify
      case And(es) => Or(es map Neg).simplify
      case Or(es) => And(es map Neg).simplify
      case N(x) => N(Neg(x)).simplify
      case _ =>
        val simplified = e.simplify
        if (simplified == e) this else Neg(simplified).simplify
    }
  }

  case class Var(name: String) extends Expr {
    override def toString = name

    def simplify = this
  }

  case class Const(const: Boolean) extends Expr {
    def prettyString = if (const) "T" else "F"

    def simplify = this
  }

}
