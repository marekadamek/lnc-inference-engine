package nclogic.parser

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

  object Tokens {

    val EqToken = "<->"
    val RImplToken = "->"
    val LImplToken = "<-"
    val NegToken = "!"
    val AndToken = "&"
    val OrToken = "|"
    val LParToken = "("
    val RParToken = ")"
    val TrueToken = "T"
    val FalseToken = "F"
    val ChangeToken = "C"
    val NextToken = "N"
    val GloballyToken = "G"
    val FinallyToken = "F"

    val validTokens = Set(
      EqToken,
      RImplToken,
      LImplToken,
      NegToken,
      AndToken,
      OrToken,
      LParToken,
      RParToken,
      TrueToken,
      FalseToken,
      ChangeToken,
      NextToken,
      GloballyToken,
      FinallyToken)

    val validCharacters = validTokens.flatMap(_.toCharArray)

    def isCharValid(c: Char) = validCharacters.contains(c) || c.isLower
  }
}

