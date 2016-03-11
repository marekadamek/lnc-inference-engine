package nclogic.model

import nclogic.model.Types.{Expr, N, Neg, Var}

case class Node(state: Set[Expr], private val getSuccessors: () => Set[Node]) {
  lazy val successors = getSuccessors()

  override def equals(o: Any) = o match {
    case other: Node => state == other.state
    case _ => false
  }
}

case class Graph(start: Node) extends Traversable[Node] {

  override def foreach[U](f: (Node) => U): Unit = {
    def loop(todo: List[Node], traversed: List[Node]): Unit = todo match {
      case Nil => ()
      case n :: ns if traversed contains n => loop(ns, traversed)
      case n :: ns =>
        f(n)
        loop(ns ++ n.successors, n :: traversed)
    }

    loop(List(start), Nil)
  }
}

object HistoryGraphFactory {

  def create(dnf: Set[Set[Expr]]) = {

    def getVar(e: Expr): Var = e match {
      case Var(x) => Var(x)
      case Neg(x) => getVar(x)
      case N(x) => getVar(x)
    }

    val vars = for (and <- dnf; e <- and) yield getVar(e)

    def createNode(state: Set[Expr]): Node = {
      Node(state, () => dnf flatMap {
        getValuations(vars, _)
          .map(nextStep)
          .filter(_.isDefined)
          .map(_.get)
          .filterNot(_ == state)
          .map(createNode)
      })
    }

    Graph(Node(Set.empty, () => dnf flatMap {
      getValuations(vars, _).map(createNode)
    }))
  }


  def nextStep(state: Set[Expr]): Option[Set[Expr]] = {
    val (ns, vars) = state partition {
      _.isInstanceOf[N]
    }

    def loop(ns: Set[Expr], vars: Set[Expr]): Set[Expr] = {
      if (ns.isEmpty) vars
      else {
        val next = ns.head.asInstanceOf[N].e
        val neg = Neg(next).simplify
        if (vars contains neg) loop(ns.tail, vars - neg + next)
        else loop(ns.tail, vars)
      }
    }

    val nextState = loop(ns, vars)
    if (nextState != state) Some(nextState) else None
  }

  private def generate(vars: List[Expr]): Set[Set[Expr]] = vars match {
    case Nil => Set.empty
    case v :: Nil => Set(Set(v), Set(Neg(v)))
    case v :: vs =>
      generate(vs) flatMap { tail =>
        Set(tail + v, tail + Neg(v))
      }
  }

  private def getValuations(vars: Set[Var], formula: Set[Expr]): Set[Set[Expr]] = {
    val freeVariables = vars filterNot { v => formula.contains(v) || formula.contains(Neg(v)) }

    if (freeVariables.isEmpty)
      Set(formula)
    else generate(freeVariables.toList) map {
      _ ++ formula
    }
  }
}
