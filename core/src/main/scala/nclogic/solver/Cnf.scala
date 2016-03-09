package nclogic.solver

import nclogic.model.Types._

/**
 * Conjunctive Normal Form
 * In Boolean logic, a formula is in conjunctive normal form (CNF) or clausal normal form if it is a conjunction
 * of clauses, where a clause is a disjunction of literals; otherwise put, it is an AND of ORs
 * @param andList
 */
class Cnf(val andList: Set[Set[Expr]]) {

  import Cnf._

  private def simplify = new Cnf(andList.filterNot(_.containsContradictoryTerms))

  override def toString = andList.map("(" + _.map(_.toString).mkString(" | ") + ")").mkString(" & ")

  def isContradictory = andList.filter(_.size == 1).flatten.containsContradictoryTerms

  def resolute: Cnf = {
    if (isContradictory) this
    else {
      findPair(andList, (or1: Set[Expr], or2: Set[Expr]) =>
        or1.exists(p => or2.contains(Neg(p))) || or2.exists(p => or1.contains(Neg(p)))
      ) match {
        case Some((or1, or2)) =>
          val (t1, t2) = (or1 ++ or2).findContradictoryTerms.get
          val head = or1 ++ or2 - t1 - t2
          val rest = andList.filterNot(l => l == or1 || l == or2)
          new Cnf(rest + head).simplify.resolute

        case _ => this
      }
    }
  }

}

object Cnf {
  def apply(expr: Expr): Cnf = {
    def convert(expr: Expr): Expr = expr match {
      // f((e1 & e2) | e) -> f(e1 | e) & f(e2 | e)
      case Or(es) => es.find(_.isInstanceOf[And]) match {
        case Some(and@And(ands)) =>
          val rest = es.filterNot(_ == and)
          And(ands.map(a => convert(Or(rest + a)))).simplify
        case _ => expr
      }
      // proceed conversion recursively
      case And(es) => And(es.map(convert)).simplify
      case _ => expr
    }


    def toSet(expr: Expr): Set[Set[Expr]] = expr match {
      case And(es) => es.flatMap(toSet)
      case Or(es) => Set(es.flatMap(toSet).flatten)
      case x: Expr => Set(Set(x))
    }

    new Cnf(toSet(convert(expr.simplify))).simplify
  }

  private def findPair[T](list: Iterable[T], p: (T, T) => Boolean): Option[(T, T)] = {
    (for {
      x <- list
      y <- list
      if x != y
    } yield (x, y))
      .find(e => p(e._1, e._2))
  }

  private def areNegations = (p: Expr, q: Expr) => p == Neg(q) || q == Neg(p)

  implicit class TermList(terms: Set[Expr]) {
    def containsContradictoryTerms = findContradictoryTerms.isDefined

    def findContradictoryTerms = findPair(terms, areNegations)
  }

}
