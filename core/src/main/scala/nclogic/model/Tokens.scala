package nclogic.model

object Tokens {

  val EqToken = "<=>"
  val RImplToken = "=>"
  val LImplToken = "<="
  val NegToken = "!"
  val AndToken = "&"
  val OrToken = "|"
  val LParToken = "("
  val RParToken = ")"
  val TrueToken = "T"
  val FalseToken = "F"
  val ChangeToken = "C"
  val NextToken = "N"

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
    NextToken)

  val validCharacters = validTokens.flatMap(_.toCharArray)

  def isCharValid(c: Char) = validCharacters.contains(c) || c.isLower


}
