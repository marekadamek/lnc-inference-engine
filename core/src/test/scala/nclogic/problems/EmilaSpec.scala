package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.HistoryGraph
import nclogic.model.converters.DnfConverter
import nclogic.parser.{FormulaReader, Parser}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class EmilaSpec extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve simple implication example" in {
    val source = Source.fromURL(getClass.getResource("/problems/emila.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val humansEndingKillTribe = "inprison & !dragon & killtribe & !attack & !hassword & !firstao & !secondao & !monks & !dead & !village & !battle & joinhumans & !endelves & !endhumans & !enddragon"
    val elvesEndingGetSword = "inprison & !dragon & !killtribe & !attack & !hassword & !firstao & !secondao & !monks & !dead & !village & !battle & !joinhumans & !endelves & !endhumans & !enddragon"
    val getKilledByDragon = "inprison & !dragon & !killtribe & attack & !hassword & !firstao & !secondao & !monks & !dead & !village & !battle & !joinhumans & !endelves & !endhumans & !enddragon"
    //tutaj stan poczatkowy to ten z mnichami
    val killTheDragon = "!inprison & !dragon & !killtribe & attack & hassword & firstao & secondao & monks & !dead & !village & !battle & !joinhumans & !endelves & !endhumans & !enddragon"

    testPath(graph, humansEndingKillTribe, "endhumans & !hassword")
    testPath(graph, elvesEndingGetSword, "endelves & hassword")
    testPath(graph, getKilledByDragon, "dead")
    testPath(graph, killTheDragon, "enddragon")

  }

  private def testPath(graph: HistoryGraph, from: String, to: String): Unit = {
    println("Testing for:")
    println("From: " + from)
    println("To: " + to)

    val initialDoNotAttack = DnfConverter.convert(Parser.parse(from).get)
    val toAnnihilationOrder = DnfConverter.convert(Parser.parse(to).get)

    println()

    //val path = graph.findPath(initialDoNotAttack, toAnnihilationOrder)
    //if (path.isEmpty) println("Path not found :(")
    //else PrettyPrintUtil.printPath(graph.findPath(initialDoNotAttack, toAnnihilationOrder))

    println()
    println()
  }
}
