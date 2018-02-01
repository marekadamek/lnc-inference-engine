//package nclogic.problems
//
//import nclogic.LncInferenceEngine
//import nclogic.model.converters.DnfConverter
//import nclogic.parser.{FormulaReader, Parser}
////import nclogic.utils.PrettyPrintUtil
//import org.scalatest.{FlatSpec, Matchers}
//
//import scala.io.Source
//
//class Implication2Spec extends FlatSpec with Matchers {
//
//  "LNC Inference engine" should "solve simple implication example 2" in {
//    val source = Source.fromURL(getClass.getResource("/problems/implication2.txt"))
//    val formula = Parser.parse(FormulaReader.read(source))
//    val graph = LncInferenceEngine.getHistoryGraph(formula.get)
//
//    val from = DnfConverter.convert(Parser.parse("a & !b & !c").get)
//    val to = DnfConverter.convert(Parser.parse("c").get)
//
//    //val path = graph.findPath(from, to)
//
//    //PrettyPrintUtil.printPath(path)
//  }
//}
