package nclogic.sat

import com.sun.java.swing.plaf.windows.WindowsTreeUI.ExpandedIcon
import nclogic.model.expr._
import nclogic.model.{LNC, NormalFormConverter}

import scala.collection.mutable

case class Node(terms: Set[Expr], toExpand: Set[Expr])


case class TableAux(expr: Expr) {
  private var toDo = List(Node(Set.empty, Set(expr)))
  private val visited = mutable.Set.empty[Node]

  def isBaseTerm(e: Expr): Boolean = e match {
    case True | False | Var(_) | Not(Var(_)) => true
    case _ => false
  }

  private def isNot(e: Expr): Boolean = e match {
    case Not(_) => true
    case _ => false
  }

  private def setTrue(es: List[Expr], terms: Set[Expr], acc: Set[Expr] = Set.empty): Set[Expr] = es match {
    case Nil => acc
    case e :: tail =>
      TableAux.setTrue(e, terms) match {
        case False | Next(False, _) =>
          Set(False)
        case True | Next(True, _) =>
          setTrue(tail, terms, acc)
        case x =>
          setTrue(tail, terms, acc + x)
      }
  }

  private def expand(n: Node): List[Node] = {
    val (terms, nonTerms) = n.toExpand.partition(_.isTerm)
    if (terms.nonEmpty) {
      val (nt1, t1) = n.terms.partition(isNot)
      val (nt2, t2) = terms.partition(isNot)
      if (terms.contains(False) || Expr.isContradictory(terms) || t1.exists(t => nt2.contains(Not(t))) || t2.exists(t => nt1.contains(Not(t)))) {
        Nil
      } else {
        //val ts = setTrue(nonTerms.toList, terms)
        // if (ts.contains(False)) {
        //   Nil
        // } else {
        List(Node(n.terms ++ terms, nonTerms))
        // }
      }
    } else {
      val ands = n.toExpand.filter {
        case And(_) | Not(Or(_)) => true
        case _ => false
      }

      if (ands.nonEmpty) {
        val andEs = ands.flatMap {
          case And(x) => x
          case Not(Or(x)) => x.map(e => !e)
        }

        val es = setTrue(andEs.toList, n.terms)
        List(Node(n.terms, es ++ (n.toExpand -- ands)))
      } else {
        val or = n.toExpand.find {
          case Or(_) | Not(And(_)) => true
          case _ => false
        }

        val e = or.get
        val newExpand = n.toExpand - e
        val list = e match {
          case Or(es) =>
            es.size match {
              case 2 => List(es.head, es.tail.head)
              case _ => List(es.head, Or(es.tail))
            }

          case Not(And(es)) =>
            es.size match {
              case 2 => List(!es.head, !es.tail.head)
              case _ => List(!es.head, Or(es.tail.map(e => !e)))
            }
        }

        list.map(e => Node(n.terms, newExpand + e))
      }

    }
  }

  // }


  private def perms(data: List[List[Expr]]): List[List[Expr]] = data match {
    case Nil => Nil
    case x :: Nil => x.map(v => List(v))
    case x :: xs =>
      val a = perms(xs)
      for {
        v <- x
        p <- a
      } yield v :: p
  }

  def getNext(): Option[Expr] = {
    var result = Option.empty[Expr]
    var i = 0
    while (toDo.nonEmpty && result.isEmpty) {
      val node = toDo.head
      toDo = toDo.tail
      if (!visited.contains(node)) {
        visited += node
        if (node.toExpand.isEmpty) {
          result = LNC.basicSimplify(And(node.terms)) match {
            case False => None
            case e => Some(e)
          }
        } else {
          toDo = expand(node) ++ toDo
        }
      } else {
        i += 1
      }
    }
    result
  }
}

object TableAux {

  private def setTrueAnd(es: List[Expr], terms: Set[Expr]): List[Expr] = es match {
    case Nil => Nil
    case head :: tail =>
      setTrue(head, terms) match {
        case True => True :: setTrueAnd(tail, terms)
        case False => List(False)
        case e => e :: setTrueAnd(tail, terms)
      }
  }

  private def setTrueOr(es: List[Expr], terms: Set[Expr]): List[Expr] = es match {
    case Nil => Nil
    case head :: tail =>
      setTrue(head, terms) match {
        case False => False :: setTrueOr(tail, terms)
        case True => List(True)
        case e => e :: setTrueOr(tail, terms)
      }
  }

  def setTrue(e: Expr, terms: Set[Expr]): Expr = e match {
    case And(es) =>
      setTrueAnd(es.toList, terms) match {
        case Nil => False
        case e1 :: Nil => e1
        case es1 => LNC.basicSimplify(And(es1.toSet))
      }

    case Or(es) =>
      setTrueOr(es.toList, terms) match {
        case Nil => False
        case e1 :: Nil => e1
        case es1 => LNC.basicSimplify(Or(es1.toSet))
      }

    case Eq(e1, e2) =>
      val x1 = setTrue(e1, terms) match {
        case x: Not => NormalFormConverter.moveNegInside(x)
        case x => x
      }

      val x2 = setTrue(e2, terms) match {
        case x: Not => NormalFormConverter.moveNegInside(x)
        case x => x
      }

      NormalFormConverter.moveNegInside(LNC.basicSimplify(Eq(x1, x2)))

    case _ if terms.contains(e) => True
    case Var(x) if terms.contains(Not(Var(x))) => False
    case Not(Var(x)) if terms.contains(Var(x)) => False
    case Next(Var(x), l) if terms.contains(Not(Next(Var(x), l))) => False
    case Not(Next(Var(x), l)) if terms.contains(Next(Var(x), l)) => False

    case _ => e
  }


  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Expr] = TableAux(expr).getNext()

  def solveAll(expr: Expr): List[Expr] = {
    val tableAux = TableAux(expr)
    var result = List.empty[Expr]
    var elem = tableAux.getNext()

    while (elem.isDefined) {
      result = elem.get :: result
      elem = tableAux.getNext()

    }
    result
  }
}
