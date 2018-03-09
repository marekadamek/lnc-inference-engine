package nclogic.model.expr

case class And(es: List[Expr]) extends Expr {
  //require(es == es.distinct, "And contains duplicates\n" + es + "\n" + es.distinct)

  override def toString = "(" + es.map(_.toString).mkString(" & ") + ")"
  override def toLatexString: String = "(" + es.map(_.toLatexString).mkString(" \\land ") + ")"

  def isAtomic = false

  def simplify = {
    var simplified = es.map(_.simplify).foldLeft(List.empty[Expr]) {(result, e) => e match {
      case And(others) => (result ++ others).distinct
      case _ => if (result.contains(e)) result else result ++ List(e)
    }}

    if (simplified.size == 1) es.head
    else {
      simplified = simplified.filterNot(_ == True)
      if (simplified.contains(False)) False
      else {
        val isContradictory = simplified.exists(e1 => simplified.exists(e2 => e1 == Neg(e2).simplify || Neg(e1).simplify == e2))
        if (isContradictory) False
        else if (es == simplified) this
        else And(simplified).simplify
      }
    }
  }

  override lazy val getTerms: List[Expr] = es.flatMap(_.getTerms)

  override def equals(o: Any) = o match {
    case And(others) => es.toSet == others.toSet
    case _ => false
  }

  override def hashCode(): Int = es.map(_.hashCode()).sum

  override def replaceVariables(s: SubstitutionSet): Expr = And(es.map(_.replaceVariables(s)))

  override def baseTerms: Set[Expr] = es.flatMap(_.baseTerms).toSet

  override def level: Int = es.map(_.level).max
}