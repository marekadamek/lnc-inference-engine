package lnc.mc

import java.nio.file.Paths

import lnc.AppConfig
import lnc.external.ModelCheckingSPOTExporter
import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.sat.SatSolvers

object Kripke extends App with AppConfig {
  val p1 = Var("p1")
  val p2 = Var("p2")

  val nodes = 40000
  val model = KripkeStructureGenerator.generateRandomStructure(
    nodesCount = nodes,
    initialNodes = nodes / 2,
    edgesCount = (nodes * nodes * 0.001).toInt,
    prepositions = 40
  )

  //val model = new KripkeStructure(model0.nodes.mapValues(n => n.copy(terms = n.terms + p1)), model0.edges)
  val spec1 = NormalFormConverter.convertToLN(C(5, p1 | p2))
  val spec2 = NormalFormConverter.convertToLN(C(2, !p1 | p2))
  val spec3 = NormalFormConverter.convertToLN(C(2, p1 | !p2))
  val spec4 = NormalFormConverter.convertToLN(C(2, !p1 | !p2))
  //val spec5 = NormalFormConverter.convertToLN(C(2, p1 | p2))


//    var model = new KripkeStructure
//    model = model.addNode(KripkeStructureNode(1, Set(p1), true))
//    model = model.addNode(KripkeStructureNode(2, Set(True), true))
//    model = model.addNode(KripkeStructureNode(3, Set(True), true))
//    model = model.addNode(KripkeStructureNode(4, Set(p1), true))
//    model = model.addEdge(1, 1)
//    model = model.addEdge(1, 2)
//    model = model.addEdge(2, 3)
//    model = model.addEdge(2, 4)
//    model = model.addEdge(3, 4)
//    model = model.addEdge(4, 1)
//    val specification = NormalFormConverter.convertToLN(p1 | N(2,p1))


//    var model = new KripkeStructure
//    model = model.addNode(KripkeStructureNode(1, Set(!p1, p2), true))
//    model = model.addNode(KripkeStructureNode(2, Set(!p1, p2), true))
//    model = model.addNode(KripkeStructureNode(3, Set(True), true))
//    model = model.addNode(KripkeStructureNode(4, Set(p1, !p2), true))
//    model = model.addEdge(1, 1)
//    model = model.addEdge(1, 3)
//
//    model = model.addEdge(2, 1)
//    model = model.addEdge(2, 2)
//    model = model.addEdge(2, 4)
//
//    model = model.addEdge(3, 1)
//    model = model.addEdge(3, 2)
//    model = model.addEdge(3, 3)
//    model = model.addEdge(3, 4)
//
//
//    model = model.addEdge(4, 2)
//    model = model.addEdge(4, 3)
//    model = model.addEdge(4, 4)

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  //val specification = NormalFormConverter.convertToLN(p1 | N(2, p1))
  val contraExample = LNCModelChecker.verify(model, spec1, SatSolvers.tableAux, Some(100))
  println(contraExample)


 // ModelCheckingSMVExporter.convert(model, specification, Paths.get(targetDir, "test.smv"))
 // ModelCheckingSPOTExporter.convert(model, List(spec1), Paths.get(spotTargetDir, "test.spot"))


}