package nclogic.model.expr

case class Or(es: Set[Expr]) extends Expr {

  override lazy val toString: String = {
    if (es.size == 1) es.head.toString
    else s"(${es.mkString(" | ")})"
  }

  override lazy val boolString: String = {
    if (es.size == 1) es.head.boolString
    else s"(${es.map(_.boolString).mkString(" | ")})"
  }

  lazy val simplify: Expr = {
    val simplified = es.map(_.simplify).foldLeft(Set.empty[Expr]) {
      case (set, Or(e)) => set ++ e
      case (set, False) => set
      case (set, Next(False, _)) => set
      case (set, e) => set + e
    }

    val containsTrue = simplified.exists {
      case True => true
      case Next(True, _) => true
      case _ => false
    }

    if (containsTrue || Expr.isContradictory(simplified))
      True
    else {
      simplified.size match {
        case 0 => False
        case 1 => simplified.head
        case _ => Or(simplified)
      }
    }
  }

}

object Or {
  def apply(es: Expr*): Expr = if (es.size == 1) es.head else Or(es.toSet)

  def formSet[T <: Expr](es: Set[T]): Or = Or(es.map(_.asInstanceOf[Expr]))
}