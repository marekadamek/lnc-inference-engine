package nclogic.model.expr

import nclogic.LncInferenceEngine
import nclogic.model.converters.{CnfConverter, DnfConverter}

trait Expr {
  def simplify: Expr

  def isAtomic: Boolean

  lazy val dnf = DnfConverter.convert(this)
  lazy val cnf = CnfConverter.convert(this)

  def matches(e: Expr) = LncInferenceEngine.isTautology(this -> e)

  def getTerms: List[Expr] = if (isAtomic) List(this) else simplify.getTerms

  def and(e: Expr): Expr = Expr.and(this, e)
  def or(e: Expr): Expr  = Expr.or(this, e)
  def xor(e: Expr): Expr  = Xor(this, e)
  def isEqualTo(e: Expr): Expr  = Eq(this, e).simplify
  def implies(e: Expr): Expr  = Impl(this, e).simplify
  def not: Expr  = Neg(this).simplify
  def next: Expr = Next(this).simplify
  def change: Expr = Change(this).simplify
  def always: Expr = Always(this).simplify

  def &(e: Expr) = and(e)
  def |(e:Expr) = or(e)
  def ^(e: Expr) = xor(e)
  def unary_! = not
  def ->(e: Expr) = implies(e)
  def `<-`(e: Expr) = e.implies(this)
  def <-> (e: Expr) = isEqualTo(e)

  def replaceVariables(s: SubstitutionSet): Expr

}

trait TemporalExpr extends Expr {
  def e: Expr
}

object Expr {
  implicit def stringToVar(name: String) = Var(name)

  def and(es: Expr*): Expr = if (es.isEmpty) False else And(es.toList).simplify

  def or(es: Expr*): Expr = if (es.isEmpty) False else Or(es.toList).simplify
}