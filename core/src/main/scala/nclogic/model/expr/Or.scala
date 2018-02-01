package nclogic.model.expr

case class Or(es: List[Expr]) extends Expr {
   override def toString = "(" + es.map(_.toString).mkString(" | ") + ")"

   def isAtomic = false

   def simplify = {
     var simplified = es.map(_.simplify).foldLeft(List.empty[Expr]) {(result, e) => e match {
       case Or(others) => (result ++ others).distinct
       case _ => if (result.contains(e)) result else result ++ List(e)
     }}

     if (simplified.size == 1) es.head
     else {
       simplified = simplified.filterNot(_ == False)
       if (simplified.contains(True)) True
       else {
         val isContradictory = simplified.exists(e1 => simplified.exists(e2 => e1 == Neg(e2) || Neg(e1) == e2))
         if (isContradictory) True
         else if (es == simplified) this
         else Or(simplified).simplify
       }
     }
   }

   override lazy val getTerms = es.flatMap(_.getTerms)

   override def equals(o: Any) = o match {
     case Or(others) => es.toSet == others.toSet
     case _ => false
   }

   override def hashCode(): Int = es.map(_.hashCode()).sum

  override def replaceVariables(s: SubstitutionSet): Expr = Or(es.map(_.replaceVariables(s)))
}
