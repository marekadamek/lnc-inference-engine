package nclogic.model

object Types {

  sealed trait Expr {
    def simplify: Expr
  }

  private def areContradictory(e1: Expr, e2: Expr) = e1 == Neg(e2) || Neg(e1) == e2

  case class And(e1: Expr, e2: Expr) extends Expr {
    override def toString = s"($e1 & $e2)"

    def simplify: Expr = (e1, e2) match {
      case (Const(false), _) => Const(false)
      case (_, Const(false)) => Const(false)
      case (Const(true), _) => e2.simplify
      case (_, Const(true)) => e1.simplify
      case (a, b) if areContradictory(a, b) => Const(false)
      case _ =>
        val (es1, es2) = (e1.simplify, e2.simplify)
        if (es1 == e1 && es2 == e2) this
        else And(es1, es2).simplify
    }

    override def equals(any: Any) = any match {
      case o: And => Set(e1, e2) == Set(o.e1, o.e2)
      case _ => false
    }
  }

  case class Or(e1: Expr, e2: Expr) extends Expr {
    override def toString = s"($e1 | $e2)"

    def simplify: Expr = (e1, e2) match {
      case (Const(true), _) => Const(true)
      case (_, Const(true)) => Const(true)
      case (Const(false), _) => e2.simplify
      case (_, Const(false)) => e1.simplify
      case (a, b) if areContradictory(a, b) => Const(true)
      case _ =>
        val (es1, es2) = (e1.simplify, e2.simplify)
        if (es1 == e1 && es2 == e2) this
        else Or(es1, es2).simplify
    }

    override def equals(any: Any) = any match {
      case o: Or => Set(e1, e2) == Set(o.e1, o.e2)
      case _ => false
    }
  }

  case class Impl(t1: Expr, t2: Expr) extends Expr {
    override def toString = s"${t1.toString} => ${t2.toString}"

    def simplify = Or(Neg(t1), t2).simplify
  }

  case class Eq(e1: Expr, e2: Expr) extends Expr {
    override def toString = s"$e1 <=> $e2"

    def simplify = Or(And(e1, e2), And(Neg(e1), Neg(e2))).simplify

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
     * N(!a) <=> !N(a)
     */
    def simplify: Expr = e match {
      case Or(e1, e2) => Or(N(e1), N(e2)).simplify
      case And(e1, e2)=> And(N(e1), N(e2)).simplify
      case Impl(p, q) => Impl(N(p), N(q)).simplify
      case Eq(p, q) => Eq(N(p), N(q)).simplify
      case Neg(e1) => Neg(N(e1)).simplify
      case _ => N(e.simplify)
    }
  }

  case class Neg(e: Expr) extends Expr {
    override def toString = "!" + e

    def simplify: Expr = e match {
      case Const(b) => Const(!b)
      case Var(_) => this
      case Neg(x) => x.simplify
      case And(e1, e2) => Or(Neg(e1), Neg(e2)).simplify
      case Or(e1, e2) => And(Neg(e1), Neg(e2)).simplify
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
