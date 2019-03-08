//package nclogic.sat
//
//import nclogic.model.expr._
//import nclogic.model.{LNC, NormalFormConverter, PrefixFormulaConverter}
//
//import scala.collection.mutable
//
//case class FormulaGraph(formula: Expr) {
//  private val graph = new Graph[Expr, Set[Set[Expr]]]
//  private var visited = mutable.Set.empty[Expr]
//  private var toDo = List(formula)
//  private var done = false
//
//  private def getVar(expr: Expr): Option[Var] = {
//    def loop(toDo: List[Expr]): Option[Var] = toDo match {
//      case Nil => None
//      case e :: tail =>
//        e match {
//          case True | False | Next(_, _) => loop(tail)
//
//          case v: Var => Some(v)
//
//          case Not(x) => loop(x :: tail)
//
//          case And(es) =>
//            loop(es.toList ++ tail)
//
//          case Or(es) =>
//            loop(es.toList ++ tail)
//
//          case Eq(e1, e2) =>
//            loop(e1 :: e2 :: tail)
//
//          case Impl(e1, e2) =>
//            loop(e1 :: e2 :: tail)
//        }
//    }
//
//    loop(List(expr))
//  }
//
//  private def advance(expr: Expr): Expr = {
//    def loop(e: Expr): Expr = e match {
//      case False => False
//
//      case True | Var(_) | Not(Var(_)) => True
//
//      case Next(x, l) => N(l - 1, x)
//
//      case Not(x) => Not(loop(x))
//
//      case And(es) =>
//        And(es.map(loop))
//
//      case Or(es) =>
//        Or(es.map(loop))
//
//      case Eq(e1, e2) =>
//        Eq(loop(e1), loop(e2))
//
//    }
//
//    LNC.basicSimplify(loop(expr))
//  }
//
//
//  private def isBaseTerm(e: Expr) = e match {
//    case Var(_) | Not(Var(_)) => true
//    case _ => false
//  }
//
//  private def isAlpha(e: Expr) = e match {
//    case And(_) | Not(Or(_)) | Not(Eq(_, _)) | Not(Impl(_, _)) => true
//    case _ => false
//  }
//
//  private def isBeta(e: Expr) = e match {
//    case Or(_) | Eq(_, _) | Impl(_, _) | Not(And(_)) => true
//    case _ => false
//  }
//
//
//  def memoize[I, O](f: I => O): I => O = {
//    val cache = new mutable.HashMap[I, O]()
//    (key: I) => cache.getOrElseUpdate(key, f(key))
//  }
//
//
//  val solveAll: Expr => Map[Expr, Set[Set[Expr]]] = memoize {
//    case (expr: Expr) =>
//      var nodes = List((Set.empty[Expr], Set(expr)))
//
//      val results = mutable.Set.empty[(Expr, Set[Expr])]
//
//      while (nodes.nonEmpty) {
//        val (terms, expressions) = nodes.head
//        nodes = nodes.tail
//
//        if (expressions.contains(False) || Expr.isContradictory(expressions)) {
//          println("oksx")
//        } else {
//          //val (newTerms, nonTerms) = expressions.partition(isBaseTerm)
//          //if (newTerms.nonEmpty) {
//          //            val withTrue = FormulaGraph.setTrue(Expr.and(nonTerms), newTerms) match {
//          //              case And(es) => es
//          //              case e => Set(e)
//          //            }
//
//          // nodes = (terms ++ newTerms, nonTerms) :: nodes
//          //} else {
//          val (alphas, nonAlphas) = expressions.partition(isAlpha)
//
//          if (alphas.nonEmpty) {
//            val newExpressions = alphas.flatMap {
//              case And(es) => es
//              case Not(Or(es)) => es.map(!_)
//              case Not(Eq(e1, e2)) => Set(e1 | e2, !e1 | !e2)
//              case Not(Impl(e1, e2)) => Set(e1, !e2)
//            }
//
//            nodes = (terms, nonAlphas ++ newExpressions) :: nodes
//
//          } else {
//            expressions.find(isBeta) match {
//              case Some(beta) =>
//                val toExpand = beta match {
//                  case Not(And(es)) => es.map(!_)
//                  case Or(es) => es
//                  case Eq(e1, e2) => Set(e1 & e2, !e1 & !e2)
//                  case Impl(e1, e2) => Set(!e1, e2)
//                }
//
//                val others = expressions - beta
//                val newNodes = toExpand.map(te => (terms, others + te))
//                nodes = newNodes.toList ++ nodes
//
//              case None =>
//                val (terms, nonTerms) = expressions.partition(isBaseTerm)
//                val next = if (nonTerms.isEmpty) True else Expr.and(nonTerms)
//                results += ((next, terms))
//            }
//          }
//        }
//        // }
//      }
//
//      results.groupBy(_._1).mapValues(_.map(_._2).toSet)
//  }
//
//  def expand(parent: Expr): Unit = {
//    if (!visited(parent)) {
//      visited += parent
//
//      val all = solveAll(parent)
//
//      all.foreach {
//        case (e, edges) =>
//          graph.addEdge(parent, advance(e), edges)
//      }
//    }
//  }
//
//
//  val visited2 = mutable.Set.empty[Expr]
//
//  private def buildGraph(): Unit = {
//    while (toDo.nonEmpty) {
//      val parent = toDo.head
//      toDo = toDo.tail
//
//      if (!visited2.contains(parent)) {
//        visited2 += parent
//
//        parent match {
//          case True =>
//          case _ =>
//            expand(parent)
//
//            val succ = graph.getSuccessors(parent)
//
//            for {
//              (child, _) <- succ
//            } {
//              toDo = child :: toDo
//            }
//        }
//      }
//    }
//  }
//
//  def simpleForm: Expr = {
//    if (!done) {
//      buildGraph()
//      done = true
//    }
//
//    def loop(expr: Expr): Expr = {
//      val es = graph.getSuccessors(expr).map {
//        case (next, solutions) =>
//          val current = LNC.basicSimplify(Expr.or(solutions.map(Expr.and)))
//
//          next match {
//            case True => current
//            case _ =>
//              loop(next) match {
//                case Or(es) => Expr.or(es.map(N(_) & current))
//                case e => current & N(e)
//              }
//          }
//
//      }
//
//      Expr.or(es.toSet)
//    }
//
//    LNC.basicSimplify(NormalFormConverter.moveNInside(loop(formula)))
//
//  }
//
//  def minimumSimpleForm: Expr = {
//    if (!done) {
//      buildGraph()
//      done = true
//    }
//
//    def loop(expr: Expr): Expr = {
//      val es = graph.getSuccessors(expr).map {
//        case (next, solutions) =>
//          val current = Expr.and(solutions.head)
//
//          next match {
//            case True => current
//            case _ =>
//              val rest = loop(next)
//              current & N(rest)
//          }
//
//      }
//
//      Expr.or(es.toSet)
//    }
//
//    LNC.basicSimplify(NormalFormConverter.moveNInside(loop(formula)))
//
//  }
//
//
//  def prefixDNF: Expr = {
//    if (!done) {
//      buildGraph()
//      done = true
//    }
//
//    //    def loop(expr: Expr): Expr = {
//    //      val es = graph.getSuccessors(expr).map {
//    //        case (next, solutions) =>
//    //          val current = Expr.and(solutions.head)
//    //
//    //          next match {
//    //            case True => current
//    //            case _ =>
//    //              val result = loop(next) match {
//    //                case False => False
//    //                case True => current
//    //                case rest => current & rest & NormalFormConverter.moveNInside(rest)
//    //              }
//    //              LNC.basicSimplify(result)
//    //          }
//    //
//    //      }
//    //
//    //      Expr.or(es.toSet)
//    // }
//
//
//    //val f = loop(formula)
//    // LNC.basicSimplify(f)
//
//    False
//  }
//}
//
//object FormulaGraph {
//
//  private def setTrueAnd(es: List[Expr], term: Expr, acc: Set[Expr]): Expr = {
//    es match {
//      case Nil => if (acc.isEmpty) True else Expr.and(acc)
//      case head :: tail =>
//        setTrue(head, term) match {
//          case True => setTrueAnd(tail, term, acc)
//          case False => setTrueAnd(Nil, term, Set(False))
//          case e if acc.contains(!e) => setTrueAnd(Nil, term, Set(False))
//          case e => setTrueAnd(tail, term, acc + e)
//        }
//    }
//  }
//
//  private def setTrueOr(es: List[Expr], term: Expr, acc: Set[Expr]): Expr = {
//    es match {
//      case Nil => if (acc.isEmpty) False else Expr.or(acc)
//      case head :: tail =>
//        setTrue(head, term) match {
//          case False => setTrueOr(tail, term, acc)
//          case True => setTrueOr(Nil, term, Set(True))
//          case e if acc.contains(!e) => setTrueOr(Nil, term, Set(True))
//          case e => setTrueOr(tail, term, acc + e)
//        }
//    }
//  }
//
//  def setTrue(e: Expr, vs: Set[Expr]): Expr = e match {
//    case True | False => e
//    case _ =>
//      if (vs.isEmpty) {
//        e
//      } else {
//        val (v, tail) = (vs.head, vs.tail)
//        setTrue(setTrue(e, v), tail)
//      }
//
//  }
//
//  def setTrue(e: Expr, v: Expr): Expr = {
//    val result = e match {
//      case True => True
//      case False => False
//
//      case Var(x) =>
//        v match {
//          case Var(x1) if x == x1 => True
//          case Not(Var(x1)) if x == x1 => False
//          case _ => e
//        }
//      case Not(Var(x)) =>
//        v match {
//          case Not(Var(x1)) if x == x1 => True
//          case Var(x1) if x == x1 => False
//          case _ => e
//        }
//
//      case Not(x) =>
//        setTrue(x, v) match {
//          case True => False
//          case False => True
//          case Not(x1) => x1
//          case x1 => Not(x1)
//        }
//
//      case Next(_, _) => e
//
//      case And(es) =>
//        setTrueAnd(es.toList, v, Set.empty)
//
//      case Or(es) =>
//        setTrueOr(es.toList, v, Set.empty)
//
//      case Eq(e1, e2) =>
//        (setTrue(e1, v), setTrue(e2, v)) match {
//          case (True, x2) => x2
//          case (False, x2) => !x2
//          case (x1, True) => x1
//          case (x1, False) => !x1
//          case (x1, x2) => Eq(x1, x2)
//        }
//    }
//
//    result
//  }
//}
//
//object Test extends App {
//  val a = Var("a")
//  val b = Var("b")
//
//  val input = PrefixFormulaConverter.convert(C(5, a))
//
//  println(input)
//
//}
//
