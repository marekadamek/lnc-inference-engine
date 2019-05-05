//package nclogic.binary
//
//import nclogic.model.BinaryGraph
//
//object BinaryValidatorWithCorrection {
//
//  def delta(e1: List[Boolean], e2: List[Boolean]): Int = e1.zip(e2).count(p => p._1 != p._2)
//
//  def validateAndCorrect(list: List[Boolean], graph: BinaryGraph): Option[List[Boolean]] = {
//    val n = graph.baseTermsSize
//    val d = graph.level
//
//    def loop(list: List[Boolean], node: List[Boolean], acc: List[List[Boolean]]): Option[List[List[Boolean]]] = list match {
//      case Nil => Some(acc.reverse)
//      case _ =>
//        val sublist = list.take(n)
//        val successors = graph.getSuccessors(node)
//        successors.find(s => delta(s.takeRight(n), sublist) == 0) match {
//          case Some(s) => loop(list.drop(n), s, sublist :: acc)
//          case None => successors.filter(s => delta(s.takeRight(n), sublist) == 1).toList match {
//            case Nil => None
//            case s :: Nil => loop(list.drop(n), s, s.takeRight(n) :: acc)
//            case _ => None
//          }
//        }
//    }
//
//
//    val start = list.take(n * d)
//    val nodes = graph.getAllNodes.find(node => delta(node, start) == 0) match {
//      case Some(node) => loop(list.drop(n * d), node, List(node))
//      case None => graph.getAllNodes.filter(node => delta(node, start) == 1).toList match {
//        case Nil => None
//        case node :: Nil => loop(list.drop(n), node, List(node))
//        case _ => None
//      }
//    }
//
//    nodes.map(_.flatten)
//
//  }
//
//  def validateCyclesAndCorrect(list: List[Boolean], cycles: List[List[Boolean]], startNode: List[Boolean]): Option[List[Boolean]] = {
//    val cyclesMap = cycles.map(c => (c, List(c, startNode).flatten))
//
//    def loop(list: List[Boolean], acc: List[List[Boolean]]): Option[List[List[Boolean]]] = list match {
//      case Nil => Some((startNode :: acc).reverse)
//      case _ =>
//        val cycleOp = cyclesMap.find(c => {
//          val sublist = list.take(c._2.length)
//          sublist.lengthCompare(c._2.length) == 0 && delta(sublist, c._2) == 0
//        }) match {
//          case Some(c1) => Some(c1)
//          case None => cyclesMap.filter(c => {
//            val sublist = list.take(c._2.length)
//            sublist.lengthCompare(c._2.length) == 0 && delta(sublist, c._2) == 1
//          }) match {
//            case c1 :: Nil => Some(c1)
//            case _ => None
//          }
//        }
//
//        cycleOp match {
//          case Some(c) =>
//            loop(list.drop(c._1.length), c._1 :: acc)
//          case None =>
//            if (list.lengthCompare(startNode.length) == 0) {
//              val d = delta(list, startNode)
//              if (d <= 1) Some((startNode :: acc).reverse)
//              else None
//            } else None
//        }
//    }
//
//    loop(list, Nil).map(_.flatten)
//  }
//
//}
