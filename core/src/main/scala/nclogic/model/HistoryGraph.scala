package nclogic.model

import nclogic.model.expr._


case class Edge(from: Expr, to: Expr)

case class HistoryGraph(private val valuationsExpr: Expr) {

  val valuations: List[Expr] = valuationsExpr match {
    case Or(es) => es
    case e => List(e)
  }

  lazy val baseTerms: Set[Expr] = valuationsExpr.baseTerms
  lazy val level: Int = valuationsExpr.level

  private val metaEdges = valuations.map(convertToMetaEdge).toSet

  def getAllNodes: Set[Expr] = {
    val compressedNodes = Set(metaEdges.map(_.from), metaEdges.map(_.to)).flatten
    val allBaseTerms = (0 until level).flatMap(i => baseTerms.map(t => createNext(t, i)))
    compressedNodes.flatMap(n => fill(n.getTerms, allBaseTerms.toList, Set(Set.empty)))
      .map(x => And(x.toList).simplify)
  }

  private def fill(terms: List[Expr], allBaseTerms: List[Expr], acc: Set[Set[Expr]]): Set[Set[Expr]] = allBaseTerms match {
    case Nil => acc
    case bt :: bts =>
      val newOnes = terms.find(t => t == bt || t == Neg(bt))
        .map(x => Set(x))
        .getOrElse(Set(bt, Neg(bt)))

      val newAcc = acc.flatMap(set => newOnes.map(no => set + no))

      fill(terms, bts, newAcc)

  }

  def getSuccessors(node: Expr): Set[Expr] = {
    val tos = metaEdges.toList.filter(e => (e.from & node).simplify != False).map(_.to)
    val advanced = advance(node)
    val result = tos.flatMap(to => {
      val lastOnes = to.getTerms.filter(_.level == level - 1)
      generateLastOnesList(lastOnes).map(lo => {
        advanced & lo
      })
    })
    result.toSet
  }

  def findPath(from: Expr, to: Expr): List[Expr] = {

    def bfs(todo: List[List[Expr]]): List[Expr] = todo match {
      case Nil => Nil
      case curr :: rest =>
        if ((curr.head & to).simplify != False) curr.reverse
        else {
          val newPaths = getSuccessors(curr.head)
            .filterNot(curr.contains)
            .map(_ :: curr)
          bfs(rest ++ newPaths)
        }
    }

    bfs(List(List(from)))
  }
  private def generateLastOnesList(lastOnes: List[Expr]): List[Expr] = {
    var result = List[Expr](True)

    baseTerms.foreach(bt => {
      lastOnes.find(p => p.baseTerms == Set(bt)) match {
        case Some(lo) => result = result.map(_ & lo)
        case None =>
          result = List(bt, Neg(bt)).map(t => createNext(t, level - 1)).flatMap(t => {
            result.map(_ & t)
          })
      }
    })

    result
  }

  private def convertToMetaEdge(e: Expr): Edge = {
    val terms = e.getTerms
    val from = And(terms.filter(_.level < level))
    val to = advance(e)

    Edge(from, to)
  }

  private def advance(e: Expr): Expr = e.getTerms.filter(_.level > 0).map(_.asInstanceOf[TemporalExpr].e) match {
    case Nil => True
    case e1 :: Nil => e1
    case es => And(es)
  }

  private def createNext(term: Expr, level: Int): Expr = level match {
    case 0 => term
    case 1 => N(term)
    case _ => createNext(N(term), level - 1)
  }

}