package nclogic

import nclogic.binary.{BinaryGenerator, BinaryValidator, BinaryValidatorWithCorrection}
import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr._
import nclogic.model.{BinaryGraph, HistoryGraph}
import nclogic.sat.Sat

import scala.util.Random


object CorrectionTest extends App {

  val a = Var("a")
  val b = Var("b")
  val c = Var("b")

  val formula = (!a -> C(b)) & (!b -> C(a))

  val sat = Sat.solve(CnfConverter.convert(formula))
  val graph = HistoryGraph(sat)
  val bGraph = BinaryGraph(graph)

  def applyNoise(list: List[Boolean], ber: Float): List[Boolean] = {
    val toAlter = (list.length * ber).toInt
    val indices = Random.shuffle(list.indices.toList).take(toAlter)

    def mod(list: List[Boolean], indices: List[Int]): List[Boolean] = indices match {
      case Nil => list
      case i :: is =>
        val elem = list(i)
        mod(list.updated(i, !elem), is)
    }

    mod(list, indices)
  }

  val All = 1000
  val length = 10000

  println("Length: " + BinaryGenerator.generate(length, bGraph).length)

  val encoder = List.fill(17)(Random.nextBoolean())

  def genNoise(B: List[Boolean], ber: Float): List[Boolean] = {
    val BM = applyNoise(B, ber)
    if (BinaryValidator.validate(BM, bGraph))
      genNoise(B, ber)
    else BM
  }

  List(0.0001f, 0.00025f, 0.0005f, 0.00075f, 0.001f).foreach(ber => {
    var totalTime = 0L
    var goodRead = 0
    var errorDetected = 0
    var badRead = 0

    for (i <- 1 to All) {
      val B = BinaryGenerator.generate(length, bGraph)
      val BM = genNoise(B, ber)

      val result = time.measureTime {
        BinaryValidatorWithCorrection.validateAndCorrect(BM, bGraph)
      }

      if (result.result.isEmpty) {
        errorDetected += 1
      }
      else {
        if (result.result.get == B)
          goodRead += 1
        else
          badRead += 1
      }

      totalTime += result.microTime

    }

    println(this.getClass.getName)
    println("BER " + ": " + ber)
    println("detections " + ": " + errorDetected)
    println("goodReads " + ": " + goodRead)
    println("bad read :( " + ": " + badRead)
    println("LNC time ms: " + totalTime / 1000)
    println()
  })
}

