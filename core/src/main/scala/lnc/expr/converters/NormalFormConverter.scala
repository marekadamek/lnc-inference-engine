package lnc.expr.converters

import lnc.LNC
import lnc.expr.{And, Change, Eq, Expr, False, Impl, N, Next, Not, Or, True, Var}
import lnc.utils.Memoize

object NormalFormConverter {

  def moveNextOutside(expr: Expr, d: Int): Expr = expr match {
    case True | False | Var(_) => expr

    case Not(x) => moveNextOutside(x, d) match {
      case Next(x1, l) => Next(Not(x1), l)
      case x1 => Not(x1)
    }

    case Next(x, l) => moveNextOutside(x, d) match {
      case Next(x1, l1) => Next(x1, l + l1)
      case x1 => Next(x1, l)
    }

    case And(es) =>
      val nOut = es.map(moveNextOutside(_, d))
      val minN = nOut.foldLeft(d) {
        case (min, Next(_, l)) if l < min => l
        case (min, Next(_, _)) => min
        case _ => 0
      }

      if (minN > 0) N(minN, And(nOut.map(_.asInstanceOf[Next]).map(n => N(n.level - minN, n.e))))
      else And(nOut)

    case Or(es) =>
      val nOut = es.map(moveNextOutside(_, d))
      val minN = nOut.foldLeft(d) {
        case (min, Next(_, l)) if l < min => l
        case (min, Next(_, l)) => min
        case _ => 0
      }

      if (minN > 0) N(minN, Or(nOut.map(_.asInstanceOf[Next]).map(n => N(n.level - minN, n.e))))
      else Or(nOut)

    case Impl(e1, e2) =>
      (moveNextOutside(e1, d), moveNextOutside(e2, d)) match {
        case (Next(x1, l1), Next(x2, l2)) =>
          val minN = Math.min(l1, l2)
          N(minN, Impl(N(l1 - minN, x1), N(l2 - minN, x2)))
        case (x1, x2) =>
          Impl(x1, x2)
      }

    case Eq(e1, e2) =>
      (moveNextOutside(e1, d), moveNextOutside(e2, d)) match {
        case (Next(x1, l1), Next(x2, l2)) =>
          val minN = Math.min(l1, l2)
          N(minN, Eq(N(l1 - minN, x1), N(l2 - minN, x2)))
        case (x1, x2) =>
          Eq(x1, x2)
      }


    case Change(x, l) => moveNextOutside(x, d) match {
      case Next(x1, l1) => Next(Change(x1, l), l1)
      case x1 => Change(x1, l)
    }

  }

  def convertToLN(expr: Expr): Expr = {
    def loop(e: Expr): Expr = e match {
      case True | False | Var(_) => e
      case Not(Not(x)) => loop(x)
      case Not(x) => Not(loop(x))
      case And(es) => And(es.map(loop))
      case Or(es) => Or(es.map(loop))
      case Impl(e1, e2) => Impl(loop(e1), loop(e2))
      case Eq(e1, e2) => Eq(loop(e1), loop(e2))
      case Next(x, l) => Next(loop(x), l)

      case Change(x, l) =>
        val arg = x match {
          case Not(neg) => neg
          case _ => x
        }

        val s = if (l > 1) loop(Change(arg, l - 1)) else loop(arg)
        loop(Eq(s, Not(Next(s, 1))))
    }

    Memoize.withCache(loop)(expr)
  }

  /**
    * N(a | b) <=> N(a) | N(b)
    * N(a & b) <=> N(a) & N(b)
    * N(a => b) <=> N(a) => N(b)
    * N(a <=> b) <=> N(a) <=> N(b)
    * N(!a) <=> !N(a) (implementation in Neg)
    */
  def moveNInside(expr: Expr): Expr = {
    def loop(e: Expr): Expr = e match {
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

    Memoize.withCache(loop)(expr)
  }

  def moveNegInside(expr: Expr): Expr = {
    def loop(e: Expr): Expr = e match {
      case True | False => e
      case _ if e.isTerm => e
      case And(es) => And(es.map(loop))
      case Or(es) => Or(es.map(loop))
      case Impl(e1, e2) => loop(Or(Set(!e1, e2)))
      case Eq(e1, e2) => Eq(loop(e1), loop(e2))
      case Next(x, l) => Next(loop(x), l)
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

    Memoize.withCache(loop)(expr)
  }

  def optimize(ln: Expr): Expr = optimize(ln, LNC.depth(ln))

  def optimize(ln: Expr, d: Int): Expr = {
    val flat = moveNextOutside(ln.simplify, d)

    def dropN(expr: Expr) = expr match {
      case Next(x, _) => x
      case And(es) => Expr.and(es.map {
        case Next(x, _) => x
        case x => x
      })
      case _ => expr
    }

    dropN(flat)
  }

  def convertToNormalForm(e: Expr): Expr = {
    val a = convertToLN(e)
    val b = moveNInside(a)
    val c = moveNegInside(b)
    c.simplify
  }
}
