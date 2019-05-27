package lnc.expr

import lnc.expr.ltl.{Always, Finally, Release, Until}

import scala.collection.mutable

/**
  * Base trait representing logic formula
  */
trait Expr {

  /**
    * Simplifies "this" to equivalent formula by applying classical logic simplification rules
    *
    * @return - simplified formula equivalent to input "this"
    */
  lazy val simplify: Expr = Expr.simplify(this)
}

class PrefixExprVisitor(expr: Expr) extends Traversable[Expr] {

  override def foreach[U](f: Expr => U): Unit = {

    def loop(elems: List[Expr]): Unit = elems match {
      case Nil =>
      case e :: es =>
        f(e)
        e match {
          case True | False | Var(_) => loop(es)
          case Not(x) => loop(x :: es)
          case And(xs) => loop(xs.toList ::: es)
          case Or(xs) => loop(xs.toList ::: es)
          case Impl(x1, x2) => loop(x1 :: x2 :: es)
          case Eq(x1, x2) => loop(x1 :: x2 :: es)
          case Next(x, _) => loop(x :: es)
          case Change(x, _) => loop(x :: es)

          case Always(x) => loop(x :: es)
          case Finally(x) => loop(x :: es)
          case Until(x1, x2) => loop(x1 :: x2 :: es)
          case Release(x1, x2) => loop(x1 :: x2 :: es)
        }
    }

    loop(List(expr))
  }
}

class InfixExprVisitor(expr: Expr) extends Traversable[Expr] {

  override def foreach[U](f: Expr => U): Unit = {

    def loop(elems: List[Expr]): Unit = elems match {
      case Nil =>
      case e :: es =>
        e match {
          case True | False | Var(_) => loop(es)
          case Not(x) => loop(x :: es)
          case And(xs) => loop(xs.toList ::: es)
          case Or(xs) => loop(xs.toList ::: es)
          case Impl(x1, x2) => loop(x1 :: x2 :: es)
          case Eq(x1, x2) => loop(x1 :: x2 :: es)
          case Next(x, _) => loop(x :: es)
          case Change(x, _) => loop(x :: es)

          case Always(x) => loop(x :: es)
          case Finally(x) => loop(x :: es)
          case Until(x1, x2) => loop(x1 :: x2 :: es)
          case Release(x1, x2) => loop(x1 :: x2 :: es)
        }
    }

    loop(List(expr))
    f(expr)
  }
}

object Expr {

  val T: Expr = True
  val F: Expr = False

  def v(name: String): Expr = Var(name)

  def and(es: Set[Expr]): Expr = {
    def andLoop(list: List[Expr], acc: Set[Expr]): Expr = list match {
      case Nil =>
        acc.size match {
          case 0 => True
          case 1 => acc.head
          case _ => And(acc)
        }

      case e :: tail => e match {
        case False => False
        case True => andLoop(tail, acc)
        case _ if isContradictory(e, acc) => False
        case And(xs) => andLoop(tail, acc ++ xs)
        case _ => andLoop(tail, acc + e)
      }
    }

    if (es.isEmpty) False
    else andLoop(es.toList, Set.empty)
  }

  def and(es: Expr*): Expr = and(es.toSet)

  def or(es: Set[Expr]): Expr = {
    def orLoop(list: List[Expr], acc: Set[Expr]): Expr = list match {
      case Nil =>
        acc.size match {
          case 0 => False
          case 1 => acc.head
          case _ => Or(acc)
        }

      case e :: tail => e match {
        case True => True
        case False => orLoop(tail, acc)
        case _ if isContradictory(e, acc) => True
        case Or(xs) => orLoop(tail, acc ++ xs)
        case _ => orLoop(tail, acc + e)
      }
    }

    if (es.isEmpty) False
    else orLoop(es.toList, Set.empty)

  }

  def or(es: Expr*): Expr = or(es.toSet)

  def not(e: Expr): Expr = e match {
    case True => False
    case False => True
    case Not(x) => x
    case _ => Not(e)
  }

  def impl(e1: Expr, e2: Expr): Expr = (e1, e2) match {
    case (False, _) => True
    case (_, True) => True
    case (True, _) => e2
    case (_, False) => Expr.not(e1)
    case (x, y) if x == y => e1
    case _ => Impl(e1, e2)
  }

  def eq(e1: Expr, e2: Expr) = (e1, e2) match {
    case (True, _) => e2
    case (False, _) => Expr.not(e2)
    case (_, True) => e1
    case (_, False) => Expr.not(e1)
    case (x, y) if x == y => True
    case (x, y) if x == Expr.not(y) => False
    case (Not(x), Not(y)) => Eq(x, y)
    case (x, y) => Eq(x, y)

  }

  /**
    * Checks whether given set of formulas is contradictory
    *
    * @param es set of LNC formulas
    * @return true if input set of formulas is contradictory, otherwise false
    */
  def isContradictory(es: Set[Expr]): Boolean = {
    if (es.size < 2) false
    else {
      val (negated, plain) = es.partition {
        case Not(_) => true
        case _ => false
      }

      plain.exists(e => negated.contains(Not(e)))
    }
  }

  /**
    * Checks whether given set of formulas is contradictory with new formula
    *
    * @param e  new LNC formula
    * @param es set of LNC formulas
    * @return true if input set of formulas is contradictory with new formula, otherwise false
    */
  def isContradictory(e: Expr, es: Set[Expr]): Boolean = {
    es.exists(x => e == Expr.not(x))
  }

  def simplify(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def andLoop(elems: List[Expr], acc: List[Expr]): Expr = elems match {
      case Nil => acc match {
        case Nil => True
        case _ => Expr.and(acc.reverse.toSet)
      }

      case x :: xs => loop(x) match {
        case False => False
        case True => andLoop(xs, acc)
        case And(xss) =>
          val newAcc = xss.toList ++ acc
          if (Expr.isContradictory(newAcc.toSet)) {
            False
          } else {
            andLoop(xs, newAcc)
          }

        case xx =>
          if (Expr.isContradictory(xx, acc.toSet)) {
            False
          } else {
            andLoop(xs, xx :: acc)
          }
      }
    }

    def orLoop(elems: List[Expr], acc: List[Expr]): Expr = elems match {
      case Nil => acc match {
        case Nil => False
        case _ => Expr.or(acc.reverse.toSet)
      }

      case x :: xs => loop(x) match {
        case True => True
        case False => orLoop(xs, acc)
        case Or(xss) =>
          val newAcc = xss.toList ++ acc
          if (Expr.isContradictory(newAcc.toSet)) {
            True
          } else {
            orLoop(xs, newAcc)
          }

        case xx =>
          if (Expr.isContradictory(xx, acc.toSet)) {
            True
          } else {
            orLoop(xs, xx :: acc)
          }
      }
    }

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case True | False => e
          case x if isTerm(x) => x

          case Not(x) => loop(x) match {
            case True => False
            case False => True
            case Not(y) => y
            case y => Not(y)
          }

          case And(es) =>
            andLoop(es.toList, Nil)

          case Or(es) =>
            orLoop(es.toList, Nil)

          case Impl(e1, e2) =>
            loop(e1) match {
              case False => True
              case True => loop(e2)
              case se1 => loop(e2) match {
                case True => True
                case False => !se1
                case se2 => Impl(se1, se2)
              }
            }

          case Eq(e1, e2) =>
            loop(e1) match {
              case True => loop(e2)
              case False => loop(!e2)
              case s1 =>
                loop(e2) match {
                  case True => s1
                  case False => loop(!s1)
                  case s2 => (s1, s2) match {
                    case _ if s1 == s2 => True
                    case _ if s1 == !s2 => False
                    case (Not(x), Not(y)) => Eq(x, y)
                    case _ => Eq(s1, s2)
                  }
                }
            }

          case Next(x, l) => loop(x) match {
            case True => True
            case False => False
            case Next(x1, l1) => Next(x1, l + l1)
            case x1 => Next(x1, l)
          }

          case Change(Next(x, l1), l) => loop(Next(Change(x, l), l1))

          case Change(x, l) => Change(loop(x), l)
        }
      })
    }

    loop(expr)
  }

  /**
    * Simplifies input formula by assigning True and False values to terms accordingly to given set of true terms
    *
    * @param e     input LNC formula
    * @param terms set of terms values to be assigned
    * @return simplifies LNC formula
    */
  def setTrue(e: Expr, terms: Set[Expr]): Expr = e match {

    case And(es) => setTrueAnd(es.toList, terms)

    case Or(es) => setTrueOr(es.toList, terms)

    case Not(x) => setTrue(x, terms) match {
      case True => False
      case False => True
      case y => !y
    }

    case Impl(e1, e2) =>
      setTrue(e1, terms) match {
        case False => True
        case True => setTrue(e2, terms)
        case se1 => setTrue(e2, terms) match {
          case True => True
          case False => !se1
          case se2 => Impl(se1, se2)
        }
      }

    case Eq(e1, e2) =>
      (setTrue(e1, terms), setTrue(e2, terms)) match {
        case (x, y) if x == y => True
        case (x, y) if x == !y => False
        case (True, y) => y
        case (False, y) => !y
        case (x, True) => x
        case (x, False) => !x
        case (x, y) => Eq(x, y)
      }

    case t if isTerm(t) && terms.contains(t) => True

    case t if isTerm(t) && terms.contains(!t) => False

    case _ if !isTerm(e) => e

    case _ => e
  }

  private def setTrueAnd(es: List[Expr], terms: Set[Expr], acc: Set[Expr] = Set.empty): Expr = es match {
    case Nil if acc.isEmpty => True
    case Nil if acc == Set(True) => True
    case Nil => Expr.and(acc - True)
    case head :: tail =>
      setTrue(head, terms) match {
        case False => False
        case e =>
          if (acc.contains(!e)) False
          else setTrueAnd(tail, terms, acc + e)
      }
  }

  private def setTrueOr(es: List[Expr], terms: Set[Expr], acc: Set[Expr] = Set.empty): Expr = es match {
    case Nil if acc.isEmpty => True
    case Nil if acc == Set(False) => False
    case Nil => Expr.or(acc - False)
    case head :: tail =>
      setTrue(head, terms) match {
        case True => True
        case e =>
          if (acc.contains(!e)) True
          else setTrueOr(tail, terms, acc + e)
      }
  }

  /**
    * Checkes whether given LNC formula is a terms (preposition or temporal proposition)
    *
    * @param e input LNC formual
    * @return true if input formula is a term
    */
  def isTerm(e: Expr): Boolean = e match {
    case Var(_) | Not(Var(_)) | Next(Var(_), _) | Not(Next(Var(_), _)) => true
    case _ => false
  }
}