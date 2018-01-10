package nclogic.model.expr

case class And(es: Set[Expr]) extends Expr {
  override def toString = "(" + es.map(_.toString).mkString(" & ") + ")"

  def isAtomic = false

  def simplify = {
    var simplified = es.map(_.simplify).foldLeft(Set.empty[Expr]) {(result, e) => e match {
      case And(others) => result ++ others
      case _ => result + e
    }}

    if (simplified.size == 1) es.head
    else {
      simplified = simplified.filterNot(_ == True)
      if (simplified.contains(False)) False
      else {
        val isContradictory = simplified.exists(e1 => simplified.exists(e2 => e1 == Neg(e2) || Neg(e1) == e2))
        if (isContradictory) False
        else if (es == simplified) this
        else And(simplified).simplify
      }
    }
  }

  override lazy val getTerms = es.flatMap(_.getTerms)

  override def equals(o: Any) = o match {
    case And(others) => es == others
    case _ => false
  }

  override def hashCode(): Int = es.map(_.hashCode()).sum
}