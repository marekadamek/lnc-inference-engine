package nclogic.parser

import scala.io.Source

object FormulaReader {

  def read(source: Source) = source
    .getLines()
    .map(_.replaceAll( """(?m)\s+$""", ""))
    .filterNot(_.startsWith("#"))
    .mkString
}
