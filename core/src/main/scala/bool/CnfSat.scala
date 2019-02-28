package bool

import nclogic.external.BoolToCNF
import nclogic.model.expr.{And, Expr, Not}

trait CnfSat extends BoolSat {

  def solveSAT(cnf: String): Option[List[Int]]

  def iterator(e: Expr): Iterator[Expr] = {
    val (cnf, vMap) = BoolToCNF.convert(e)

    val it = satIterator(cnf, vMap)

    new Iterator[Expr] {
      override def hasNext: Boolean = it.hasNext

      override def next(): Expr = {
        val terms = it.next()
          .filter(p => vMap.contains(Math.abs(p)))
          .map { v =>
            if (v < 0) Not(vMap(-v))
            else vMap(v)
          }
        And(terms.toSet)
      }
    }
  }

  protected def satIterator(cnf: String, vMap: Map[Int, Expr]): Iterator[List[Int]] = new Iterator[List[Int]] {
    private val n = cnf.indexOf(System.lineSeparator())

    private val header = cnf.take(n).split(" ")

    private var clauses = cnf.drop(n + 1)
    private val varsCount = header(2).toInt
    private var clausesCount = header(3).toInt
    private var current = Option.empty[List[Int]]

    private var calculated = false

    private def solveNext(): Option[List[Int]] = this.synchronized {
      if (calculated) {
        current
      } else {
        val firstLine = s"p cnf $varsCount $clausesCount"
        val input = firstLine + System.lineSeparator() + clauses
        current = solveSAT(input)

        if (current.isDefined) {
          val newClause = current.get
            .filter(p => vMap.contains(Math.abs(p)))
            .map(_ * -1)
            .mkString(" ") + " 0"
          clauses = newClause + System.lineSeparator() + clauses
          clausesCount += 1
        }

        calculated = true
        current
      }
    }

    override def hasNext: Boolean = this.synchronized {
      solveNext().isDefined
    }

    override def next(): List[Int] = this.synchronized {
      val result = solveNext()
      calculated = false
      result.get
    }
  }
}
