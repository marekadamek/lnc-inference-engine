package nclogic.model.expr

trait Expr {
  def simplify(implicit level: Int = this.level): Expr

  def isAtomic: Boolean

  def getTerms: Set[Expr] = if (isAtomic) Set(this) else simplify.getTerms

  def and(e: Expr): Expr = Expr.and(this, e)

  def or(e: Expr): Expr = Expr.or(this, e)

  def xor(e: Expr): Expr = Xor(this, e)

  def isEqualTo(e: Expr): Expr = Eq(this, e)

  def implies(e: Expr): Expr = Impl(this, e)

  def not: Expr = Neg(this)

  def next: Expr = Next(this)

  def globally: Expr = Globally(this)

  def change: Expr = Change(this)

  def finallyExpr: Expr = Finally(this)

  def &(e: Expr) = and(e)

  def |(e: Expr) = or(e)

  def ^(e: Expr) = xor(e)

  def unary_! = not

  def ->(e: Expr) = implies(e)

  def `<-`(e: Expr) = e.implies(this)

  def <->(e: Expr) = isEqualTo(e)

  def baseTerms: Set[Expr]

  def level: Int
}

trait TemporalExpr extends Expr {
  def e: Expr
}

object Expr {
  implicit def stringToVar(name: String) = Var(name)

  def and(es: Expr*): Expr = if (es.isEmpty) False else And(es.toSet)

  def or(es: Expr*): Expr = if (es.isEmpty) False else Or(es.toSet)

  def getNestedTerm(te: TemporalExpr): Expr = te.e match {
    case e: TemporalExpr => getNestedTerm(e)
    case _ => te.e
  }

  def areContradictoryTerms(e1: Expr, e2: Expr): Boolean = {
    if (!e1.isAtomic || !e2.isAtomic) false
    else e1 match {
      case True => e2 == False
      case False => e2 == True

      case term1: Var => e2 match {
        case neg2: Neg => term1 == neg2.e
        case g2: Globally => areContradictoryTerms(term1, getNestedTerm(g2))
        case _ => false
      }

      case neg1: Neg => e2 match {
        case term2: Var => neg1.e == term2
        case g2: Globally => areContradictoryTerms(neg1, getNestedTerm(g2))
        case _ => false
      }

      case next1: Next => e2 match {
        case next2: Next => areContradictoryTerms(next1.e, next2.e)
        case g2: Globally => areContradictoryTerms(getNestedTerm(next1), getNestedTerm(g2))
        case _ => false
      }

      case g1: Globally =>
        val nested1 = getNestedTerm(g1)
        e2 match {
          case term2: Var => areContradictoryTerms(nested1, term2)
          case neg2: Neg => areContradictoryTerms(nested1, neg2)
          case next2: Next => areContradictoryTerms(nested1, getNestedTerm(next2))
          case g2: Globally => areContradictoryTerms(nested1, getNestedTerm(g2))
          case _ => false
        }
    }
  }
}