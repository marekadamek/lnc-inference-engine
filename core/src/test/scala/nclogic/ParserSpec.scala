package nclogic

import nclogic.model.Types._
import nclogic.parser.Parser
import org.scalatest._

import scala.util.Success

class ParserSpec extends FlatSpec with Matchers {

  "Parser" should "parse constants" in {
    Parser.parse(List("T")) shouldEqual Success(Const(true))
    Parser.parse(List("F")) shouldEqual Success(Const(false))
  }

  it should "parse vars" in {
    Parser.parse(List("a")) shouldEqual Success(Var("a"))
    Parser.parse(List("variable")) shouldEqual Success(Var("variable"))
  }

  it should "parse negation" in {
    Parser.parse(List("!", "a")) shouldEqual Success(Neg(Var("a")))
  }

  it should "parse equality" in {
    Parser.parse(List("a", "<=>", "b")) shouldEqual Success(Eq(Var("a"), Var("b")))
  }

  it should "parse implication" in {
    Parser.parse(List("a", "=>", "b")) shouldEqual Success(Impl(Var("a"), Var("b")))
    Parser.parse(List("a", "<=", "b")) shouldEqual Success(Impl(Var("b"), Var("a")))
  }

  it should "parse or" in {
    Parser.parse(List("a", "|", "b", "|", "c", "|", "d")).map(_.simplify) shouldEqual Success(Or(Set(Var("a"), Var("b"), Var("c"), Var("d"))))
    Parser.parse(List("a", "|", "(", "b", "|", "(", "c", "|", "d", ")", ")")).map(_.simplify) shouldEqual Success(Or(Set(Var("a"), Var("b"), Var("c"), Var("d"))))
  }

  it should "parse and" in {
    Parser.parse(List("a", "&", "b", "&", "c", "&", "d")).map(_.simplify) shouldEqual Success(And(Set(Var("a"), Var("b"), Var("c"), Var("d"))))
    Parser.parse(List("a", "&", "(", "b", "&", "(", "c", "&", "d", ")", ")")).map(_.simplify) shouldEqual Success(And(Set(Var("a"), Var("b"), Var("c"), Var("d"))))
  }

  it should "parse brackets" in {
    Parser.parse(List("(", "a", ")")) shouldEqual Success(Var("a"))
    Parser.parse(List("(", "(", "a", ")", ")")) shouldEqual Success(Var("a"))
    Parser.parse(List("(", "a", "|", "b", ")", "&", "c")) shouldEqual Success(And(Set(Or(Set(Var("a"), Var("b"))), Var("c"))))
    Parser.parse(List("a", "&", "(","b", "|", "c",")")) shouldEqual Success(And(Set(Var("a"), Or(Set(Var("b"), Var("c"))))))
    Parser.parse(List("(", "a", "=>", "b",")", "<=>", "(", "!", "a", "|", "b", ")")) shouldEqual Success(Eq(Impl(Var("a"), Var("b")), Or(Set(Neg(Var("a")), Var("b")))))
  }

  it should "maintain operator priority" in {
    Parser.parse(List("a", "|", "b", "&", "c")) shouldEqual Success(Or(Set(Var("a"), And(Set(Var("b"), Var("c"))))))
    Parser.parse(List("a", "&", "b", "|", "c")) shouldEqual Success(Or(Set(And(Set(Var("a"), Var("b"))), Var("c"))))
  }

  it should "return parse error on wrong input" in {
    Parser.parse(List("a", "<=>")) should be a 'failure
    Parser.parse("A") should be a 'failure
  }

  it should "parse functors C, N and G" in {
    Parser.parse(List("C", "(", "a", ")")) shouldEqual Success(C(Var("a")))
    Parser.parse(List("N", "(", "a", ")")) shouldEqual Success(N(Var("a")))

    Parser.parse(List("N", "(", "a",  "&", "b", ")")) shouldEqual Success(N(And(Set(Var("a"), Var("b")))))
  }

}
