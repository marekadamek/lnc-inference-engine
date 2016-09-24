package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.DnfConverter
import nclogic.model.Types.{Neg, Var}
import nclogic.parser.{FormulaReader, Parser}
import nclogic.utils.PrettyPrintUtil
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class EmilaSpec extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve simple implication example" in {
    val source = Source.fromURL(getClass.getResource("/problems/emila.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val initialDoNotAttack = DnfConverter.convert(Parser.parse("p & !da & !ao & !ad & !rip").get).head
    val toAnnihilationOrder = DnfConverter.convert(Parser.parse("ao").get).head

    val initialAttack = DnfConverter.convert(Parser.parse("p & !da & !ao & ad & !rip").get).head
    val toDead = DnfConverter.convert(Parser.parse("rip").get).head


    PrettyPrintUtil.printPath(graph.findPath(initialDoNotAttack, toAnnihilationOrder))
    println()
    PrettyPrintUtil.printPath(graph.findPath(initialAttack, toDead))
  }
}
