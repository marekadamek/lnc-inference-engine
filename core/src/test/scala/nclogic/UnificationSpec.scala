package nclogic

import nclogic.model.expr.{Term, Predicate, SubstitutionSet, Var}
import org.scalatest._

class UnificationSpec extends FlatSpec with Matchers {

  val friend = "friend"
  val bill = Term("bill")
  val george = Term("george")
  val kate = Term("kate")
  val merry = Term("merry")
  val X = Var("X")
  val Y = Var("Y")
  val expressions = List(
    Predicate(friend, List(bill, george)),
    Predicate(friend, List(bill, kate)),
    Predicate(friend, List(bill, merry)),
    Predicate(friend, List(george, bill)),
    Predicate(friend, List(george, kate)),
    Predicate(friend, List(kate, merry))
  )

  "Unification" should "work" in {
    val goal = Predicate(friend, List(X, Y))

    System.out.println("Goal = " + goal)
    expressions.foreach(e => {
      val s = e.unify(goal, new SubstitutionSet())
      if (s != null) println(goal.replaceVariables(s))
      else println("False")
    })
  }

  it should "work with one constant" in {
    val goal = Predicate(friend, List(bill, Y))

    System.out.println("Goal = " + goal)
    expressions.foreach(e => {
      val s = e.unify(goal, new SubstitutionSet())
      if (s != null) println(goal.replaceVariables(s))
      else println("False")
    })
  }
}
