package nclogic.model

import nclogic.model.expr._
import nclogic.sat.TableAux
import time._

import scala.collection.mutable

object PrefixFormulaConverter {

  private def suffix(expr: Expr): Expr = {
    def loop(e: Expr): Option[Expr] = {
      e match {
        case True => Some(True)
        case False => Some(False)
        case Var(_) => None
        case Not(Var(_)) => None
        case Next(x, l) => Some(N(l - 1, x))
        case Not(x) => loop(x).map(Not)

        case And(es) =>
          val converted = es.map(loop).filter(_.isDefined).map(_.get)
          converted.size match {
            case 0 => None
            case 1 => Some(converted.head)
            case _ => Some(And(converted))
          }

        case Or(es) =>
          val converted = es.map(loop).filter(_.isDefined).map(_.get)
          converted.size match {
            case 0 => None
            case 1 => Some(converted.head)
            case _ => Some(Or(converted))
          }

        case Eq(e1, e2) =>
          loop(e1) match {
            case None => None
            case Some(se1) =>
              loop(e2) match {
                case None => None
                case Some(se2) => Some(Eq(se1, se2))
              }
          }

        case Impl(e1, e2) => loop(Or(Not(e1), e2))
      }
    }

    loop(expr).getOrElse(True)
  }

  private def baseForLevel(expr: Expr): Expr = {
    def loop(e: Expr): Option[Expr] = {
      e match {
        case True => Some(True)
        case False => Some(False)
        case Var(_) =>
          Some(e)
        case Not(Var(_)) =>
          Some(e)
        case Next(_, _) => None
        case Not(x) =>
          loop(x).map(Not)

        case And(es) =>
          val converted = es.map(loop).filter(_.isDefined).map(_.get)
          converted.size match {
            case 0 => None
            case 1 => Some(converted.head)
            case _ => Some(And(converted))
          }

        case Or(es) =>
          val converted = es.map(loop).filter(_.isDefined).map(_.get)
          converted.size match {
            case 0 => None
            case 1 => Some(converted.head)
            case _ => Some(Or(converted))
          }

        case Eq(e1, e2) =>
          loop(e1) match {
            case None => None
            case Some(se1) =>
              loop(e2) match {
                case None => None
                case Some(se2) => Some(Eq(se1, se2))
              }
          }

        case Impl(e1, e2) => loop(Or(Not(e1), e2))
      }
    }

    loop(expr).getOrElse(True)
  }

  def convert(e: Expr): Expr = {
    val ln = NormalFormConverter.convertToLN(e)
    val d = LNC.depth(ln)

    var s = ln
    val sufixes = for {
      _ <- 1 to d
    } yield {
      s = suffix(s)
      s
    }

    LNC.basicSimplify(Expr.and(Set(ln) ++ sufixes - True))
  }

  def convert2(e: Expr): Expr = {
    val d = LNC.depth(e)
    val ln = NormalFormConverter.convertToNormalForm(e)

    val commonBase = getCommonBase(e)

    val addons = for (i <- 0 to d) yield {
      NormalFormConverter.moveNInside(N(i, commonBase))
    }

    LNC.basicSimplify(Expr.and(addons.toSet + ln))
  }

  def getCommonBase(e: Expr): Expr = {
    val d = LNC.depth(e)
    val ln = NormalFormConverter.convertToLN(e)

    val bases = mutable.Set.empty[Expr]
    bases += baseForLevel(ln)

    var s = ln
    for (_ <- 1 to d) {
      s = suffix(s)
      bases += baseForLevel(s)
    }

    LNC.basicSimplify(Expr.and(bases.toSet))
  }

  def convert3(e: Expr): Expr = {
    val ln = timeLog("convert3 a") {
      NormalFormConverter.convertToNormalForm(NormalFormConverter.convert(e))
    }
    val d = LNC.depth(ln)

    def convertLoop(suffix: Expr, prefix: Expr, acc: Set[Expr], l: Int, solutions: Set[Set[Expr]]): Expr = suffix match {
      case False => False
      case True => LNC.basicSimplify(Expr.and(acc))
      case _ =>
        val (s, p) =
          timeLog("convert3 d " + l) {
            suffixBDD(suffix, solutions)
          }

        val px =
          if (p.isEmpty) prefix
          else prefix & N(l, Expr.or(p.map(Expr.and)))

        convertLoop(s, px, acc + N(d - l, px), l + 1, solutions ++ p)
    }

    val x = timeLog("convert3 b") {
      convertLoop(ln, True, Set(ln), 0, Set.empty)
    }

    val y = timeLog("convert3 c") {
      NormalFormConverter.convertToNormalForm(x)
    }
    LNC.basicSimplify(y)
  }


  private def setTrue(e: Expr, t: Expr) = TableAux.setTrue(e, Set(t))

  def getTerm(expr: Expr): Option[Expr] = {
    def getTermLoop(list: List[Expr]): Option[Expr] = list match {
      case Nil => None
      case head :: tail => head match {
        case Var(_) | Not(Var(_)) => Some(head)
        case Not(x) => getTermLoop(x :: tail)
        case And(es) => getTermLoop(es.toList ::: tail)
        case Or(es) => getTermLoop(es.toList ::: tail)
        case Impl(e1, e2) => getTermLoop(e1 :: e2 :: tail)
        case Eq(e1, e2) => getTermLoop(e1 :: e2 :: tail)
        case _ => getTermLoop(tail)
      }
    }

    getTermLoop(List(expr))
  }


  def suffixBDD(expr: Expr, solutions: Set[Set[Expr]]): (Expr, Set[Set[Expr]]) = expr match {
    case True | False => (expr, Set.empty)
    case _ =>

      def expand(e: Expr, terms: List[Expr]): List[(Expr, List[Expr])] = getTerm(e) match {
        case None => Nil
        case Some(t) => List((e, t :: terms), (e, !t :: terms))
      }

      def suffixBDDLoop(list: List[(Expr, List[Expr])], acc: Set[(Expr, Set[Expr])]): (Set[Expr], Set[Set[Expr]]) = list match {
        case Nil => (acc.map(_._1), acc.map(_._2))
        case (e, terms) :: es =>
          setTrue(e, terms.head) match {
            case True => (Set(True), Set.empty)
            case False => suffixBDDLoop(es, acc)
            case e1 =>
              expand(e1, terms) match {
                case Nil =>
                  suffixBDDLoop(es, acc + ((e1, terms.toSet)))
                case nodes =>
                  suffixBDDLoop(nodes ::: es, acc)
              }
          }
      }

      def dropN(e: Expr): Expr = e match {
        case Next(x, l) => N(l - 1, x)
        case And(es) => And(es.map(dropN))
        case Or(es) => Or(es.map(dropN))
        case Eq(e1, e2) => Eq(dropN(e1), dropN(e2))
        case Impl(e1, e2) => Impl(dropN(e1), dropN(e2))
        case Not(x) => Not(dropN(x))
        case _ => e
      }


      def checkSolutions(sols: List[Set[Expr]], acc: Set[(Expr, List[Expr])]):  List[(Expr, List[Expr])] = sols match {
        case Nil => acc.toList
        case s :: ss =>
          TableAux.setTrue(expr, s) match {
            case True => List((True, List.empty))
            case False => checkSolutions(ss, acc)
            case simp =>
              expand(simp, s.toList) match {
                case Nil => List((True, List.empty))
                case list => checkSolutions(ss, acc ++ list)
              }
          }
      }

      val initList = checkSolutions(solutions.toList, Set.empty) match {
        case Nil => expand(expr, List.empty)
        case x => x
      }

      if (initList.isEmpty || initList.head._1 == True) {
        (True, Set.empty)
      } else {
        val x = suffixBDDLoop(initList, Set.empty)
        val y = dropN(Expr.or(x._1))
        val z = LNC.basicSimplify(y)
        (z, x._2)
      }

  }
}
