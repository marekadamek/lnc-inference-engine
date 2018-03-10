package nclogic

import nclogic.model.expr._
import nclogic.parser.Parser
import org.scalatest._

import scala.util.Success

class ParserSpec extends FlatSpec with Matchers {

  "Parser" should "parse constants" in {
    Parser.parse(List("T")) shouldEqual Success(True)
    Parser.parse(List("F")) shouldEqual Success(False)
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
    Parser.parse(List("a", "|", "b", "|", "c", "|", "d")).map(_.simplify) shouldEqual Success(Expr.or(Var("a"), Var("b"), Var("c"), Var("d")))
    Parser.parse(List("a", "|", "(", "b", "|", "(", "c", "|", "d", ")", ")")).map(_.simplify) shouldEqual Success(Expr.or(Var("a"), Var("b"), Var("c"), Var("d")))
  }

  it should "parse and" in {
    Parser.parse(List("a", "&", "b", "&", "c", "&", "d")).map(_.simplify) shouldEqual Success(Expr.and(Var("a"), Var("b"), Var("c"), Var("d")))
    Parser.parse(List("a", "&", "(", "b", "&", "(", "c", "&", "d", ")", ")")).map(_.simplify) shouldEqual Success(Expr.and(Var("a"), Var("b"), Var("c"), Var("d")))
  }

//  it should "parse brackets" in {
//    Parser.parse(List("(", "a", ")")) shouldEqual Success(Var("a"))
//    Parser.parse(List("(", "(", "a", ")", ")")) shouldEqual Success(Var("a"))
//    Parser.parse(List("(", "a", "|", "b", ")", "&", "c")) shouldEqual Success(And(Or(Var("a"), Var("b")), Var("c")))
//    Parser.parse(List("a", "&", "(","b", "|", "c",")")) shouldEqual Success(And(Var("a"), Or(Var("b"), Var("c"))))
//    Parser.parse(List("(", "a", "=>", "b",")", "<=>", "(", "!", "a", "|", "b", ")")) shouldEqual Success(Eq(Impl(Var("a"), Var("b")), Or(Neg(Var("a")), Var("b"))))
//  }
//
//  it should "maintain operator priority" in {
//    Parser.parse(List("a", "|", "b", "&", "c")) shouldEqual Success(Or(Var("a"), And(Var("b"), Var("c"))))
//    Parser.parse(List("a", "&", "b", "|", "c")) shouldEqual Success(Or(And(Var("a"), Var("b")), Var("c")))
//  }
//
//  it should "return parse error on wrong input" in {
//    Parser.parse(List("a", "<=>")) should be a 'failure
//    Parser.parse("A") should be a 'failure
//  }
//
//  it should "parse functors C, N" in {
//    Parser.parse(List("C", "(", "a", ")")) shouldEqual Success(Change(Var("a")))
//    Parser.parse(List("N", "(", "a", ")")) shouldEqual Success(Next(Var("a")))
//    Parser.parse(List("N", "(", "a",  "&", "b", ")")) shouldEqual Success(Next(And(Var("a"), Var("b"))))
//  }

}
