//package nclogic
//
//import nclogic.binary.{BinaryGenerator, BinaryValidator, CRC}
//import nclogic.model.converters.CnfConverter
//import nclogic.model.expr._
//import nclogic.model.{BinaryGraph, HistoryGraph}
//import nclogic.sat.Sat
//
//import scala.util.Random
//
//
//object ValidationTest extends App {
//
//  val a = Var("a")
//  val b = Var("b")
//  val c = Var("b")
//
//  //val formula = !a -> N(N(a))
//  //val formula = C(a | b)
//  val formula = (!a -> C(b)) & (!b -> C(a))
//
//  val sat = Sat.solve(CnfConverter.convert(formula))
//  val graph = HistoryGraph(sat)
//  val bGraph = BinaryGraph(graph)
//
//  def applyNoise(list: List[Boolean], ber: Float): List[Boolean] = {
//    val toAlter = (list.length * ber).toInt
//    val indices = Random.shuffle(list.indices.toList).take(toAlter)
//
//    def mod(list: List[Boolean], indices: List[Int]): List[Boolean] = indices match {
//      case Nil => list
//      case i :: is =>
//        val elem = list(i)
//        mod(list.updated(i, !elem), is)
//    }
//
//    mod(list, indices)
//  }
//
//  val ber = 0.0001f
//  val All = 1000
//  val length = 10000
//
//  println("Length: " + BinaryGenerator.generate(length, bGraph).length)
//
//  val encoder = List.fill(17)(Random.nextBoolean())
//
//  List(0.0001f, 0.00025f, 0.0005f, 0.00075f, 0.001f).foreach(ber => {
//    var x = 0
//    var x_crc = 0
//    var totalTime = 0L
//    var totalCrcCalculateTime = 0L
//    var totalCrcValidationTime = 0L
//    for (i <- 1 to All) {
//      val B = BinaryGenerator.generate(length, bGraph)
//      val BM = applyNoise(B, ber)
//      val result = time.measureTime {
//        BinaryValidator.validate(BM, bGraph)
//      }
//
//      if (!result.result) {
//        x = x + 1
//      }
//      totalTime += result.microTime
//
//      val B_CRC = time.measureTime {
//        CRC.encode(B, encoder)
//      }
//      totalCrcCalculateTime += B_CRC.microTime
//
//      val BM_CRC = applyNoise(B_CRC.result, ber)
//      val R_CRC = time.measureTime {
//        CRC.validate(BM_CRC, encoder)
//      }
//      if (!R_CRC.result) {
//        x_crc = x_crc + 1
//      }
//
//      totalCrcValidationTime += R_CRC.microTime
//    }
//
//    println(this.getClass.getName)
//    println("BER " + ": " + ber)
//    println("LNC " + ": " + x)
//    println("LNC time: " + totalTime)
//    println("LNC time ms: " + totalTime / 1000)
//    println()
//    println("CRC calculation time: " + totalCrcCalculateTime / 1000)
//    println("CRC validation time: " + totalCrcValidationTime / 1000)
//    println()
//  })
//}
//
