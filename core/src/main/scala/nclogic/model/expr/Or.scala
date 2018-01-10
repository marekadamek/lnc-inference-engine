package nclogic.model.expr

case class Or(es: Set[Expr]) extends Expr {
   override def toString = "(" + es.map(_.toString).mkString(" | ") + ")"

   def isAtomic = false

   def simplify = {
     var simplified = es.map(_.simplify).foldLeft(Set.empty[Expr]) {(result, e) => e match {
       case Or(others) => result ++ others
       case _ => result + e
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
     case Or(others) => es == others
     case _ => false
   }

   override def hashCode(): Int = es.map(_.hashCode()).sum
 }
