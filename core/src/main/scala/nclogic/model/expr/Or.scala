package nclogic.model.expr

case class Or(es: Set[Expr]) extends Expr {
  override def toString: String = "(" + es.map(_.toString).mkString(" | ") + ")"

  def isAtomic = false

  private def isContradictory = {
    val atomics = es.filter(_.isAtomic)
    atomics.exists(e1 => atomics.exists(e2 => Expr.areContradictoryTerms(e1, e2)))
  }

  def simplify(implicit level: Int): Expr = {
    if (es.size == 1) es.head.simplify
    else {
      if (es.contains(True) || isContradictory) True
      else {
        var simplified = es - False
        val parts = simplified.map {
          case e if e.isAtomic => e
          case e => e.simplify
        }

        val (ors, rest) = parts.partition(_.isInstanceOf[Or])
        simplified = ors.flatMap(_.asInstanceOf[Or].es) ++ rest

        // p | (p & q) <=> p
        simplified = simplified.find(_.isAtomic) match {
          case Some(t) => simplified.filterNot(s => s.isInstanceOf[And] && s.asInstanceOf[And].es.contains(t))
          case None => simplified
        }

        simplified.filter(_.isInstanceOf[Finally]).foreach(a => {
          val e = Expr.getNestedTerm(a.asInstanceOf[Finally])
          simplified = simplified.filterNot {
            case term: Var => e == term
            case neg: Neg => e == neg
            case te: Next => e == Expr.getNestedTerm(te)
            case _ => false
          }
        })

        if (es == simplified) this
        else Or(simplified).simplify
      }
    }

  }

  override lazy val getTerms: Set[Expr] = es.flatMap(_.getTerms)

  override def equals(o: Any): Boolean = o match {
    case Or(others) => es == others
    case _ => false
  }

  override def hashCode(): Int = es.map(_.hashCode()).sum

  override def baseTerms: Set[Expr] = es.flatMap(_.baseTerms)

  override def level: Int = es.map(_.level).max
}

object Or {
  def apply(es: Expr*): Or = Or(es.toSet)
}