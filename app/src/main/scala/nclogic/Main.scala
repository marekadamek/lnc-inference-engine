package nclogic

import nclogic.model.DnfConverter
import nclogic.parser.{FormulaReader, Parser}

import scala.io.Source


object Main extends App {

  val file = "/implication.txt"
  val line = FormulaReader.read(Source.fromURL(getClass.getResource(file)))
  //val line = "(a | b) & (c | d)"
  val formula = Parser.parse(line)
  val g = LncInferenceEngine.getHistoryGraph(formula.get)


  //val from = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & !rc & !rf & !rs & !rw").get).head
  //val to = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & rc & rf & rs & rw").get).head

  val from = DnfConverter.convert(Parser.parse("a & !b & !c").get).head
  val to = DnfConverter.convert(Parser.parse("c").get).head


  val b = g.findPath(from, to)
  for (s <- b) {
    println(s)
  }

}

