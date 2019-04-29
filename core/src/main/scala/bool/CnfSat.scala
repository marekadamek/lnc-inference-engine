package bool

import nclogic.external.BoolToCNF
import nclogic.model.expr.{Expr, Not}

trait CnfSat extends BoolSat {

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


  def iterator(e: Expr): BoolSatIterator = new BoolSatIterator {
    private val (cnf, vMap) = BoolToCNF.convert(e)
    private val n = cnf.indexOf(System.lineSeparator())

    private val header = cnf.take(n).split(" ")

    private var clauses = cnf.drop(n + 1)
    private val varsCount = header(2).toInt
    private var clausesCount = header(3).toInt


    def next(): Option[Set[Expr]] = {
      val firstLine = s"p cnf $varsCount $clausesCount"
      val input = firstLine + System.lineSeparator() + clauses

      solveSAT(input).map(s => {
        val newClause = s
          .filter(p => vMap.contains(Math.abs(p)))
          .map(_ * -1)
          .mkString(" ") + " 0"
        clauses = newClause + System.lineSeparator() + clauses
        clausesCount += 1

        convertToExpr(s, vMap)
      })

    }

  }
}
