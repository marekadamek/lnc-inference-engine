package nclogic.model.expr

import nclogic.model.converters.{CnfConverter, DnfConverter}

trait Expr {
  def simplify: Expr

  def isAtomic: Boolean

  def getTerms: List[Expr] = if (isAtomic) List(this) else simplify.getTerms

  def and(e: Expr): Expr = Expr.and(this, e)
  def or(e: Expr): Expr  = Expr.or(this, e)
  def xor(e: Expr): Expr  = Xor(this, e)
  def isEqualTo(e: Expr): Expr  = Eq(this, e)
  def implies(e: Expr): Expr  = Impl(this, e)
  def not: Expr  = Neg(this)
  def next: Expr = Next(this)
  def change: Expr = Change(this)

  def &(e: Expr) = and(e)
  def |(e:Expr) = or(e)
  def ^(e: Expr) = xor(e)
  def unary_! = not
  def ->(e: Expr) = implies(e)
  def `<-`(e: Expr) = e.implies(this)
  def <-> (e: Expr) = isEqualTo(e)

  def replaceVariables(s: SubstitutionSet): Expr

  def baseTerms: Set[Expr]
  def level: Int
  def toLatexString = toString
}

trait TemporalExpr extends Expr {
  def e: Expr

  override val level: Int = e.level + 1
}

object Expr {
  implicit def stringToVar(name: String) = Var(name)

  def and(es: Expr*): Expr = if (es.isEmpty) False else And(es.toList)

  def or(es: Expr*): Expr = if (es.isEmpty) False else Or(es.toList)
}