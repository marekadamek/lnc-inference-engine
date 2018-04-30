package nclogic.model.expr

case class And(es: Set[Expr]) extends Expr {

  override def toString: String = "(" + es.map(_.toString).mkString(" & ") + ")"

  def isAtomic = false

  private def isContradictory = {
    val atomics = es.filter(_.isAtomic)
    atomics.exists(e1 => atomics.exists(e2 => Expr.areContradictoryTerms(e1, e2)))
  }

  def simplify(implicit level: Int): Expr = {
    if (es.size == 1) es.head.simplify
    else {
      if (es.contains(False) || isContradictory) False
      else {
        var simplified = es - True
        val parts = simplified.map {
          case e if e.isAtomic => e
          case e => e.simplify
        }

        val (ands, rest) = parts.partition(_.isInstanceOf[And])
        simplified = ands.flatMap(_.asInstanceOf[And].es) ++ rest

        // p & (p | q) <=> p
        simplified = simplified.find(_.isAtomic) match {
          case Some(t) => simplified.filterNot(s => s.isInstanceOf[Or] && s.asInstanceOf[Or].es.contains(t))
          case None => simplified
        }

        simplified.filter(_.isInstanceOf[Globally]).foreach(a => {
          val e = Expr.getNestedTerm(a.asInstanceOf[Globally])
          simplified = simplified.filterNot {
            case term: Var => e == term
            case neg: Neg => e == neg
            case te: Next => e == Expr.getNestedTerm(te)
            case _ => false
          }
        })

        if (es == simplified) this
        else And(simplified).simplify
      }
    }

  }

  override lazy val getTerms: Set[Expr] = es.flatMap(_.getTerms)

  override def equals(o: Any): Boolean = o match {
    case And(others) => es == others
    case _ => false
  }

  override def hashCode(): Int = es.map(_.hashCode()).sum

  override def baseTerms: Set[Expr] = es.flatMap(_.baseTerms)

  override def level: Int = if (es.isEmpty) 0 else es.map(_.level).max
}

object And {
  def apply(es: Expr*): And = new And(es.toSet)

  def fromSet[T <: Expr](es: Set[T]): And = And(es.map(_.asInstanceOf[Expr]))
}