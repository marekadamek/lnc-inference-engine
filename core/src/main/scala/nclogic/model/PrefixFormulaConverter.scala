package nclogic.model

import nclogic.model.expr._
import nclogic.sat.TableAux

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
    val ln = NormalFormConverter.convertToLN(e)

    val commonBase = getCommonBase(e)

    val addons = for (i <- 0 to d) yield {
      N(i, commonBase)
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
      val x = LNC.basicSimplify(s)
      bases += baseForLevel(s)
    }

    LNC.basicSimplify(Expr.and(bases.toSet))
  }

  def convert3(e: Expr): Expr = {
    val d = LNC.depth(e)
    val ln = NormalFormConverter.convertToNormalForm(e)

    var prefix = ln
    val addons = for (i <- 0 until d) yield {
      prefix = prefixBDD(prefix, d - i)
      NormalFormConverter.moveNInside(N(i + 1, prefix))
    }

    LNC.basicSimplify(Expr.and(addons.toSet + ln))
  }

  def getTermAtLevel(toDo: List[Expr], d: Int): Option[Expr] = toDo match {
    case Nil => None
    case head :: tail => head match {
      case Next(_, l) if l == d => Some(head)
      case Not(Next(_, l)) if l == d => Some(head)
      case And(es) => getTermAtLevel(es.toList ::: tail, d)
      case Or(es) =>  getTermAtLevel(es.toList ::: tail, d)
      case Impl(e1, e2) =>  getTermAtLevel(e1 :: e2 :: tail, d)
      case Eq(e1, e2) =>  getTermAtLevel(e1 :: e2 :: tail, d)
      case _ =>
        getTermAtLevel(tail, d)
    }
  }

  def prefixBDD(e: Expr, d: Int): Expr = {
    getTermAtLevel(List(e), d) match {
      case None => e
      case Some(t) =>
        TableAux.setTrue(e, Set(t)) match {
          case True => True
          case pos =>
            TableAux.setTrue(e, Set(!t)) match {
              case True => True
              case neg => prefixBDD(Expr.or(pos, neg), d)
            }
        }
    }
  }
}
