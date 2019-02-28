//package nclogic.model
//
//import nclogic.model.converters.CnfConverter
//import nclogic.model.expr._
//import nclogic.sat.Sat
//
//
//case class Edge(from: Expr, to: Expr)
//
//case class HistoryGraph(private val valuationsExpr: Expr) {
//
//  val valuations: List[Expr] = valuationsExpr match {
//    case False => Nil
//    case Or(es) => es.toList
//    case e => List(e)
//  }
//
//  lazy val baseTerms: Set[Expr] = valuationsExpr.baseTerms
//  lazy val level: Int = valuationsExpr.level
//
//  private val metaEdges = valuations.map(convertToMetaEdge).toSet
//
//  private def allBaseTerms = (0 until level).flatMap(i => baseTerms.map(t => Next.createNext(t, i)))
//
//  def getAllNodes: Set[Expr] = {
//    Set(metaEdges.map(_.from), metaEdges.map(_.to)).flatten
//      .flatMap(n => fill(n.getTerms.toList, allBaseTerms.toList, Set(Set.empty)))
//      .map(x => And(x).simplify)
//  }
//
//  def getMatchingNodes(e: Expr): Set[Expr] = {
//    Set(metaEdges.map(_.from), metaEdges.map(_.to)).flatten
//      .map(n => (n & e).simplify)
//      .filterNot(_ == False)
//      .flatMap(n => fill(n.getTerms.toList, allBaseTerms.toList, Set(Set.empty)))
//      .map(x => And(x).simplify)
//  }
//
//  def getMatchingFromNodes(e: Expr): Set[Expr] = metaEdges.map(_.from)
//      .map(n => (n & e).simplify)
//      .filterNot(_ == False)
//      .flatMap(n => fill(n.getTerms.toList, allBaseTerms.toList, Set(Set.empty)))
//      .map(x => And(x).simplify)
//
//  private def fill(terms: List[Expr], allBaseTerms: List[Expr], acc: Set[Set[Expr]]): Set[Set[Expr]] = allBaseTerms match {
//    case Nil => acc
//    case bt :: bts =>
//      val newOnes = terms.find(t => t == bt || t == Not(bt))
//        .map(x => Set(x))
//        .getOrElse(Set(bt, Not(bt)))
//
//      val newAcc = acc.flatMap(set => newOnes.map(no => set + no))
//
//      fill(terms, bts, newAcc)
//
//  }
//
//  def getSuccessors(node: Expr): Set[Expr] = {
//    val tos = metaEdges.toList.filter(e => (e.from & node).simplify != False).map(_.to)
//    val advanced = advance(node)
//    val result = tos.flatMap(to => {
//      val lastOnes = to.getTerms.filter(_.level == level - 1)
//      generateLastOnesList(lastOnes.toList).map(lo => And(Set(advanced, lo)).simplify)
//    })
//    result.toSet
//  }
//
//  def findPath(from: Expr, to: Expr): List[Expr] = {
//    val fromSimplified = from.simplify
//    val toSimplified = to.simplify
//    def bfs(todo: List[List[Expr]]): List[Expr] = todo match {
//      case Nil => Nil
//      case curr :: rest =>
//        if ((curr.head & toSimplified).simplify != False) curr.reverse
//        else {
//          val newPaths = getSuccessors(curr.head)
//            .filterNot(curr.contains)
//            .map(_ :: curr)
//          bfs(rest ::: newPaths.toList)
//        }
//    }
//
//    val startNodes = getMatchingFromNodes(fromSimplified)
//    bfs(startNodes.toList.map(List(_)))
//  }
//  private def generateLastOnesList(lastOnes: List[Expr]): List[Expr] = {
//    var result = List[Expr](True)
//
//    baseTerms.foreach(bt => {
//      lastOnes.find(p => p.baseTerms == Set(bt)) match {
//        case Some(lo) => result = result.map(_ & lo)
//        case None =>
//          result = List(bt, Not(bt)).map(t => Next.createNext(t, level - 1)).flatMap(t => {
//            result.map(_ & t)
//          })
//      }
//    })
//
//    result
//  }
//
//  private def stripG(es: Set[Expr]) = es map {
//    case Globally(e) => e
//    case e => e
//  }
//
//  private def convertToMetaEdge(e: Expr): Edge = {
//    val terms = e.getTerms
//    val from = And(stripG(terms.filter(_.level < level)))
//    val to = advance(e)
//
//    Edge(from, to)
//  }
//
//  private def advance(e: Expr): Expr = {
//    val filtered = e.getTerms.filter(_.isInstanceOf[TemporalExpr]).map(_.asInstanceOf[TemporalExpr].e)
//    And(filtered)
//  }
//
//}
//
//object HistoryGraph {
//  def fromBaseExpr(expr: Expr): HistoryGraph = HistoryGraph(Sat.solve(CnfConverter.convert(expr)))
//}