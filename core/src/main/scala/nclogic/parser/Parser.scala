package nclogic.parser

import nclogic.model.Types._
import nclogic.parser.Tokenizer.Tokens._
import scala.util.Try
import nclogic._
/**
 * E -> T | F | Var | !a | (E) | E OP E | Fun(E)
 * Var -> (a-z)+
 * Fun -> C | N | G
 * OP -> & | '|' | => | <=>
 */

object Parser {

  private val TmpPrefix = "TMP-"

  private def replaceBrackets(tokens: List[String], tmp: Map[Int, List[String]]) = {
    def iterate(list: List[(String, Int)], acc: List[String], tmp: Map[Int, List[String]]): (List[String], Map[Int, List[String]]) = list match {
      case Nil => (acc, tmp)
      case _ =>
        if (list.map(_._2).exists(_ > 0)) {
          val front = list.takeWhile(_._2 == 0)
          val bracket = list.drop(front.size).takeWhile(_._2 != 0)
          val key = if (tmp.keys.isEmpty) 1 else tmp.keys.max + 1
          val value = bracket.tail.map(_._1)
          iterate(list.drop(front.size + bracket.size + 1), acc ++ front.map(_._1) ++ List(TmpPrefix + key), tmp + (key -> value))
        }
        else (acc ++ list.map(_._1), tmp)
    }

    var level = 0
    val levels = tokens.map {
      case LParToken => level += 1; level
      case RParToken => level -= 1; level
      case _ => level
    }

    iterate(tokens zip levels, List.empty, tmp)

  }

  private def handleOperator(op: String,
                             handler: (Expr, Expr) => Expr,
                             fallback: (List[String], Map[Int, List[String]]) => Expr)
                            (tokens: List[String], tmp: Map[Int, List[String]]): Expr = {

    tokens.zipWithIndex.find(_._1 == op).map({ case (_, idx) =>
      handler(parseTokens(tokens.take(idx), tmp), parseTokens(tokens.drop(idx + 1), tmp))
    }).getOrElse(fallback(tokens, tmp))
  }

  private def processBracket(key: String, map: Map[Int, List[String]]) = {
    val k = key.drop(TmpPrefix.length).toInt
    parseTokens(map(k), map - k)
  }

  private def parseTokens(tokens: List[String], tmp: Map[Int, List[String]]): Expr = {

    val (replaced, map) = replaceBrackets(tokens, tmp)

    handleOperator(EqToken, Eq,
      handleOperator(RImplToken, Impl,
        handleOperator(LImplToken, (p, q) => Impl(q, p),
          handleOperator(OrToken, (p, q) => Or(Set(p, q)),
            handleOperator(AndToken, (p, q) => And(Set(p, q)), {
              case (NegToken :: tail, map) => Neg(parseTokens(tail, map))

              case (ChangeToken :: ts :: Nil, map) =>
                C(processBracket(ts, map))

              case (NextToken :: ts :: Nil, map) =>
                N(processBracket(ts, map))

              case (FalseToken :: Nil, _) => Const(false)

              case (TrueToken :: Nil, _) => Const(true)

              case (x :: Nil, map) if x startsWith TmpPrefix =>
                processBracket(x, map)

              case (x :: Nil, _) => Var(x)

              case _ => throw new RuntimeException("parse exception")
            })))))(replaced, map)
  }

  def parse(tokens: List[String]): Try[Expr] = Try {
    parseTokens(tokens, Map.empty)
  }

  def parse(formula: String): Try[Expr] = formula :> Tokenizer.tokenize flatMap parse
}
