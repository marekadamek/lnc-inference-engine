package pl.edu.pw.elka.madamek.nclogic

import pl.edu.pw.elka.madamek.nclogic.model.Tokens

import scala.util.Try

object Tokenizer {

  def tokenize(input: String) = Try {
    tokenizeLoop(input.trim.replace(" ", "").toList, "", Nil).reverse
  }

  private def tokenizeLoop(input: List[Char], token: String, acc: List[String]): List[String] = input match {
    case Nil => acc
    case c :: cs =>
      if (!Tokens.isCharValid(c)) {
        throw new RuntimeException("Invalid character: " + c)
      }

      if (token.isEmpty && c.isLower) {
        val variable = (c :: cs.takeWhile(_.isLower)).mkString
        tokenizeLoop(input.drop(variable.length), "", variable :: acc)
      }
      else {
        val nextToken = token + c
        if (Tokens.validTokens.contains(nextToken)) {
          (nextToken, cs) match {
            case (Tokens.LImplToken, '>':: rest) => tokenizeLoop(cs, nextToken, acc)
            case _ => tokenizeLoop(cs, "", nextToken :: acc)
          }
        }
        else
          tokenizeLoop(cs, nextToken, acc)
      }
  }

}

