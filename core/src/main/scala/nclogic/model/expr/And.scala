package nclogic.model.expr

case class And(es: Set[Expr]) extends Expr {

  override lazy val toString: String = {
    if (es.size == 1) es.head.toString
    else s"(${es.mkString(" & ")})"
  }

  lazy val simplify: Expr = {
    val simplified = es.map(_.simplify).foldLeft(Set.empty[Expr]) {
      case (set, And(e)) => set ++ e
      case (set, True) => set
      case (set, Next(True, _)) => set
      case (set, e) => set + e
    }

    val containsFalse = simplified.exists {
      case False => true
      case Next(False, _) => true
      case _ => false
    }

    if (containsFalse || Expr.isContradictory(simplified))
      False
    else {
      simplified.size match {
        case 0 => True
        case 1 => simplified.head
        case _ => And(simplified)
      }
    }
  }

  override lazy val boolString: String = {
    if (es.size == 1) es.head.boolString
    else s"(${es.map(_.boolString).mkString(" & ")})"
  }

}


object And {
  def apply(es: Expr*): And = new And(es.toSet)

  def fromSet[T <: Expr](es: Set[T]): And = And(es.map(_.asInstanceOf[Expr]))
}
