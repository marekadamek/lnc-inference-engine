package nclogic.model

import nclogic.model.expr._

import scala.collection.mutable

object NormalFormConverter {

  private val cache = mutable.Map.empty[Expr, Expr]


  def   convertToLN(e: Expr): Expr =
    cache.getOrElseUpdate(e,
      e match {
        case True | False | Var(_) => e
        case Not(Not(x)) => convertToLN(x)
        case Not(x) => Not(convertToLN(x))
        case And(es) => And(es.map(convertToLN))
        case Or(es) => Or(es.map(convertToLN))
        case Impl(e1, e2) => Impl(convertToLN(e1), convertToLN(e2))
        case Eq(e1, e2) => Eq(convertToLN(e1), convertToLN(e2))
        case Next(x, l) => Next(convertToLN(x), l)

        case Change(x, l) =>
          val arg = x match {
            case Not(neg) => neg
            case _ => x
          }

          val s = if (l > 1) convertToLN(Change(arg, l - 1)) else convertToLN(arg)
          convertToLN(s <-> !N(s))
      }
    )

  def moveNInside(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case True | Next(True, _) => True
          case False | Next(False, _) => False
          case _ if e.isTerm => e
          case Not(x) => Not(loop(x))
          case And(es) => And(es.map(loop))
          case Or(es) => Or(es.map(loop))
          case Impl(e1, e2) => Impl(loop(e1), loop(e2))
          case Eq(e1, e2) => Eq(loop(e1), loop(e2))

          case Next(x, l) => x match {
            case Next(x1, l1) => loop(Next(x1, l + l1))
            case Not(x1) => loop(Not(Next(x1, l)))
            case And(es) => loop(And(es.map(Next(_, l).asInstanceOf[Expr])))
            case Or(es) => loop(Or(es.map(Next(_, l).asInstanceOf[Expr])))
            case Impl(e1, e2) => loop(Impl(Next(e1, l), Next(e2, l)))
            case Eq(e1, e2) => loop(Eq(Next(e1, l), Next(e2, l)))
          }
        }
      })
    }

    loop(expr)
  }

  def moveNegInside(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case True | False => e
          case _ if e.isTerm => e
          case And(es) => And(es.map(loop))
          case Or(es) => Or(es.map(loop))
          case Impl(e1, e2) => loop(Or(!e1, e2))
          case Eq(e1, e2) => Eq(loop(e1), loop(e2))

          case Not(x) => x match {
            case True | Next(True, _) => False
            case False | Next(True, _) => True
            case Not(x1) => loop(x1)
            case And(es) => loop(Or(es.map(!_)))
            case Or(es) => loop(And(es.map(!_)))
            case Impl(e1, e2) => loop(And(e1, !e2))
            case Eq(e1, e2) => loop(Eq(!e1, e2))
            case Next(_, l) => e
          }
        }
      })
    }

    loop(expr)
  }

  def andOr(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case _ if e.isTerm => e
          case And(es) => And(es.map(loop))
          case Or(es) => Or(es.map(loop))
          case Impl(e1, e2) => Impl(loop(e1), loop(e2))
          case Eq(e1, e2) => Eq(loop(e1), loop(e2))

          case Not(x) => x match {
            case True | Next(True, _) => False
            case False | Next(True, _) => True
            case Not(x1) => loop(x1)
            case And(es) => loop(Or(es.map(Not(_).asInstanceOf[Expr])))
            case Or(es) => loop(And(es.map(Not(_).asInstanceOf[Expr])))
            case Impl(e1, e2) => loop(And(e1, Not(e2)))
            case Eq(e1, e2) => loop(Eq(Not(e1), e2))
          }
        }
      })
    }

    loop(expr)
  }

  def eliminateEqAndImpl(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case And(es) => And(es.map(loop))
          case Or(es) => Or(es.map(loop))
          case Not(Not(x)) => loop(x)
          case Not(x) => Not(loop(x))
          case Next(x, l) => Next(loop(x), l)
          case Impl(e1, e2) =>
            val ne1 = loop(e1) match {
              case Not(x) => x
              case x => Not(x)
            }

            Or(ne1, loop(e2))

          case Eq(e1, e2) =>
            val se1 = loop(e1)
            val ne1 = se1 match {
              case Not(x) => x
              case x => Not(x)
            }
            val se2 = loop(e2)
            val ne2 = se2 match {
              case Not(x) => x
              case x => Not(x)
            }

            Or(And(se1, se2), And(ne1, ne2))

          case _ => e
        }
      })
    }

    loop(expr)
  }

  def eliminateNeg(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case Not(Not(x)) => loop(x)
          case Not(And(es)) => Or(es.map(Not).map(loop))
          case Not(Or(es)) => And(es.map(Not).map(loop))
          case And(es) => And(es.map(loop))
          case Or(es) => Or(es.map(loop))
          case Next(e, l) => Next(loop(e), l)
          case _ => e
        }
      })
    }

    loop(expr)
  }

  def andToOr(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case And(es) =>
            val looped = es.map(loop)
            es.map(loop).find(_.isInstanceOf[Or]).map(_.asInstanceOf[Or]) match {
              case None => And(looped)
              case Some(or) =>
                val rest = looped - or
                loop(Or(or.es.map(e => And(rest + e).asInstanceOf[Expr])))
            }
          case Or(es) => Or(es.map(loop))
          case Not(x) => Not(loop(x))
          case Next(x, l) => Next(loop(x), l)
          case _ => e
        }
      })
    }

    loop(expr)
  }

  def convert(e: Expr): Expr = convertToLN(e)

  def convertToNormalForm(e: Expr): Expr = {
    val a = convertToLN(e)
    val b = moveNInside(a)
    val c = moveNegInside(b)
    LNC.basicSimplify(c)
  }
}
