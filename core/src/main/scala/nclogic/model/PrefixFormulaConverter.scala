package nclogic.model

import nclogic.model.expr._

object PrefixFormulaConverter {

  private def getVar(expr: Expr): Option[Var] = {
    def loop(toDo: List[Expr]): Option[Var] = toDo match {
      case Nil => None
      case e :: tail =>
        e match {
          case True | False | Next(_, _) => loop(tail)

          case v: Var => Some(v)

          case Not(x) => loop(x :: tail)

          case And(es) =>
            loop(es.toList ++ tail)

          case Or(es) =>
            loop(es.toList ++ tail)

          case Eq(e1, e2) =>
            loop(e1 :: e2 :: tail)

          case Impl(e1, e2) =>
            loop(e1 :: e2 :: tail)
        }
    }

    loop(List(expr))
  }

  private def getTerm (expr: Expr, d: Int): Option[Expr] = {
    if (d == 0) {
      getVar(expr)
    } else {
      def loop(toDo: List[Expr]): Option[Expr] = toDo match {
        case Nil => None
        case e :: tail =>
          e match {
            case True | False | Var(_) => loop(tail)

            case Next(Var(_), l) if l == d => Some(e)

            case Next(x, l) =>
              getTerm(x, l-1) match {
                case Some(t) => Some(N(l, t))
                case None => loop(tail)
              }

            case Not(x) => loop(x :: tail)

            case And(es) =>
              loop(es.toList ++ tail)

            case Or(es) =>
              loop(es.toList ++ tail)

            case Eq(e1, e2) =>
              loop(e1 :: e2 :: tail)

            case Impl(e1, e2) =>
              loop(e1 :: e2 :: tail)
          }
      }

      loop(List(expr))
    }

  }

  def convert(e: Expr) = {
    val depth = LNC.depth(e)
    val base = NormalFormConverter.convert(e)

    for(d <- depth to 1) {
      println(getTerm(base, d))
    }
  }
}
