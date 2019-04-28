package bool

import nclogic.external.BoolToCNF
import nclogic.model.expr.{Expr, Not}

trait CnfSat extends BoolSat {

  def getSolution(e: Expr): Option[Set[Expr]] = {
    val (cnf, vMap) = BoolToCNF.convert(e)
    solveSAT(cnf).map(convertToExpr(_, vMap))
  }

  private def convertToExpr(solution: List[Int], vMap: Map[Int, Expr]): Set[Expr] = {
    val terms = solution
      .filter(p => vMap.contains(Math.abs(p)))
      .map { v =>
        if (v < 0) Not(vMap(-v))
        else vMap(v)
      }
    terms.toSet
  }

  def solveSAT(cnf: String): Option[List[Int]]


  def getAllSolutions(e: Expr): Set[Set[Expr]] = {
    val (cnf, vMap) = BoolToCNF.convert(e)
    val n = cnf.indexOf(System.lineSeparator())

    val header = cnf.take(n).split(" ")

    var clauses = cnf.drop(n + 1)
    val varsCount = header(2).toInt
    var clausesCount = header(3).toInt
    var solutions = Set.empty[List[Int]]

    var calculated = false

    while (!calculated) {
      val firstLine = s"p cnf $varsCount $clausesCount"
      val input = firstLine + System.lineSeparator() + clauses

      solveSAT(input) match {
        case None => calculated = true
        case Some(s) =>
          solutions = solutions + s

          val newClause = s
            .filter(p => vMap.contains(Math.abs(p)))
            .map(_ * -1)
            .mkString(" ") + " 0"
          clauses = newClause + System.lineSeparator() + clauses
          clausesCount += 1
      }
    }

    solutions.map(convertToExpr(_, vMap))
  }
}
