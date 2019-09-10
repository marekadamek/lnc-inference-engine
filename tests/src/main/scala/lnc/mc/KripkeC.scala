package lnc.mc

import java.nio.file.Paths

import lnc.AppConfig
import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.ltl.Always
import lnc.external.{ModelCheckingCTLSMVExporter, ModelCheckingLTLSPOTExporter, ModelCheckingSMVExporter}
import lnc.kripke.{KripkeStructure, KripkeStructureNode}
import lnc.sat.SatSolvers

object KripkeC extends App with AppConfig {

  val p1 = Expr.v("p1")
  val p2 = Expr.v("p2")

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  var model = new KripkeStructure
  model = model.addNode(KripkeStructureNode(1, Set(p1), true))
  model = model.addNode(KripkeStructureNode(2, Set(!p1), true))
  model = model.addEdge(1, 2)
  model = model.addEdge(2, 1)
  val specification = NormalFormConverter.convertToNormalForm(p1)

  val result = LNCModelChecker.verify(model, specification, SatSolvers.dpllLike, Some(100))

  model.prettyPrint()


  println(result)


}

object KripkeTrue extends App with AppConfig {

  val p1 = Expr.v("p1")
  val p2 = Expr.v("p2")

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  var model = new KripkeStructure
  model = model.addNode(KripkeStructureNode(1, Set(p1), true))
  model = model.addEdge(1, 1)
  val specification = NormalFormConverter.convertToNormalForm(p1)

  ModelCheckingLTLSPOTExporter.convert(model, List(specification), Paths.get(spotTargetDir, s"test.spot"))
  ModelCheckingCTLSMVExporter.convert(model, List(specification), Paths.get(targetDir, s"test.smv"))

  val result = LNCModelChecker.verify(model, specification, SatSolvers.dpllLike, Some(100))

  model.prettyPrint()


  println(result)


}

object KripkeASd extends App with AppConfig {

  val p1 = Expr.v("p1")

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  var model = new KripkeStructure
  model = model.addNode(KripkeStructureNode(1, Set(!p1), true))
  model = model.addNode(KripkeStructureNode(2, Set(p1), true))
  model = model.addEdge(1, 2)
  model = model.addEdge(2, 2)
  val specification = NormalFormConverter.convertToNormalForm(N(p1))

  ModelCheckingLTLSPOTExporter.convert(model, List(specification), Paths.get(spotTargetDir, s"test.spot"))
  ModelCheckingCTLSMVExporter.convert(model, List(specification), Paths.get(targetDir, s"test.smv"))

  val result = LNCModelChecker.verify2(model, List(specification), SatSolvers.dpllLike)

  model.prettyPrint()


  println(result)


}

object KripkeRand extends App with AppConfig {

  val p = Var("p")
  val q = Var("q")
  val r = Var("r")

  private val targetDir = config.getString("nusmvMCFilesDir")
  private val spotTargetDir = config.getString("spotMCFilesTargetDir")

  var model = new KripkeStructure
  model = model.addNode(KripkeStructureNode(1, Set(q), true))
  model = model.addNode(KripkeStructureNode(2, Set(!p, !r), true))
  model = model.addNode(KripkeStructureNode(3, Set(!p,!q), true))
  model = model.addNode(KripkeStructureNode(4, Set(!q), true))
  model = model.addNode(KripkeStructureNode(5, Set(p, !q, !r), true))
  model = model.addNode(KripkeStructureNode(6, Set(!p), true))

  model = model.addEdge(1, 6)
  model = model.addEdge(2, 1)
  model = model.addEdge(2, 2)
  model = model.addEdge(2, 3)
  model = model.addEdge(3, 3)
  model = model.addEdge(4, 1)
  model = model.addEdge(5, 1)
  model = model.addEdge(6, 5)

  val e1 = Always((r <-> !N(r)) <-> (N(r) <-> N(2, r)))
  val e2 = Always(!q | (N(p) & N(q)) | (N(q) <-> !N(2, q)))

  ModelCheckingLTLSPOTExporter.convert(model, List(e1,e2), Paths.get(spotTargetDir, s"example.spot"))
  ModelCheckingSMVExporter.convert(model,  List(e1,e2), Paths.get(targetDir, s"example.smv"))

  model.prettyPrint()
  List(e1,e2).foreach(println)
}