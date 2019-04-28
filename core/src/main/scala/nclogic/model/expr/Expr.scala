package nclogic.model.expr

trait Expr {
  /** *
    *
    * @return expression in normal form
    */
  def simplify: Expr

  def boolString: String

  def &(e: Expr): Expr = Expr.and(this, e)

  def |(e: Expr): Expr = Expr.or(this, e)

  def unary_! = this match {
    case Not(x) => x
    case _ => Not(this)
  }

  def ->(e: Expr) = Impl(this, e)

  def `<-`(e: Expr) = Impl(e, this)

  def <->(e: Expr) = Eq(this, e)

  def isTerm: Boolean = this match {
    case Var(_) | Not(Var(_)) | Next(Var(_), _) | Not(Next(Var(_), _)) => true
    case _ => false
  }
}

object Expr {
  implicit def stringToVar(name: String): Expr = Var(name)

  def and(es: Set[Expr]): Expr = {
    es.size match {
      case 0 => False
      case 1 => es.head
      case _ => And(es)
    }
  }

  def and(es: Expr*): Expr = and(es.toSet)

  def or(es: Set[Expr]): Expr = {
    es.size match {
      case 0 => False
      case 1 => es.head
      case _ => Or(es)
    }
  }

  def or(es: Expr*): Expr = or(es.toSet)

  def isContradictory(es: Set[Expr]): Boolean = {
    if (es.size < 2) false
    else {
      val (negated, plain) = es.partition {
        case Not(_) => true
        case _ => false
      }

      plain.find(e => negated.contains(Not(e)))
//        .map(x => {
//          println(x)
//          x
//        })
        .isDefined


    }
  }
}