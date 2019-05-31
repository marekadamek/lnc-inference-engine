package lnc

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.Paths

import lnc.expr._
import lnc.expr.converters.PrefixFormulaConverter
import lnc.expr.ltl._
import lnc.external.{ModelCheckingLTLSPOTExporter, ModelCheckingSMVExporter, PltlExporter}
import lnc.kripke.{KripkeStructure, LNCToKripkeStructureConverter}
import lnc.mc.{LNCFormulaGenerator, LNCModelChecker, LTLFormulaGenerator}
import lnc.sat.{SatSolvers, TableAux}

object LNCByLTL extends App with AppConfig {


  val a = Var("a")
  val formula = !a -> N(2, a)

  val spec = !a -> Finally(a)

  val (model1, time1) = time.measureTime {
    LNCToKripkeStructureConverter.convert(formula, SatSolvers.tableAux)
  }

  val (model2, time2) = time.measureTime {
    LNCToKripkeStructureConverter.convert(formula, SatSolvers.dpllLike)
  }

  model1.prettyPrint()

  model2.prettyPrint()

}

object LNCByLTL_Random extends App with AppConfig {

  private val spotTargetDir = config.getString("spotMCFilesTargetDir")
  private val nusmvMCFilesDir = config.getString("nusmvMCFilesDir")

  val n = 1
  val d = 1
  val size = 5

  val vars = (1 to n).map(i => Var(s"p$i")).toList

  var i = 0
  while (i < 10) {
    val lnc = LNCFormulaGenerator.generateRandomFormulas(1, vars, d, size)


    val vars2 = LNCModelChecker.getVars(lnc.head).toList.map(_.asInstanceOf[Var])

    if (vars2.nonEmpty) {
      val formulas = LTLFormulaGenerator.generateRandomFormulas(10, vars2, size / 2)

      val pltlOut = formulas.map(f => PltlExporter.convert(List(Always(lnc.head) & !f))).mkString("\n")
      val file = Paths.get("/Users/marek/phd/files/pltl/mc", s"random$i.pltl").toFile
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(pltlOut)
      bw.close()

      val model = LNCToKripkeStructureConverter.convert(lnc.head, SatSolvers.dpllLike)
      ModelCheckingLTLSPOTExporter.convert(model, formulas, Paths.get(spotTargetDir, s"ltl/random$i.spot"))
      ModelCheckingSMVExporter.convert(model, formulas, Paths.get(nusmvMCFilesDir, s"ltl/random$i.smv"))

      i += 1
    }
  }
}

object LNCAsModel_Comparison extends App with AppConfig {

  val n = 10
  val size = 10

  val vars = (1 to n).map(i => Var(s"p$i")).toList

  val count = 10
  for (d <-4 to 4) {
    val rows = LNCFormulaGenerator.generateRandomFormulas(count, vars, d, size).zipWithIndex.map {
      case (formula, idx) =>
        val (model1, time1) = time.measureTime {
          LNCToKripkeStructureConverter.convert(formula, SatSolvers.tableAux)
        }

//        val (model2, time2) = time.measureTime {
//          LNCToKripkeStructureConverter.convert(formula, SatSolvers.dpllLike)
//        }

        //    println("c")
        //    val (model3, time3) = time.measureTime {
        //      new KripkeStructure
        //      LNCToKripkeStructureConverter.convertFull(formula, SatSolvers.dpllLike)
        //    }

        (
          model1.nodes.size,
          model1.edges.map(_._2.size).sum,
          time1.seconds
//          model2.nodes.size,
//          model2.edges.map(_._2.size).sum,
//          time2.seconds //,
          //      model3.nodes.size,
          //      model3.edges.map(_._2.size).sum,
          //      time3.seconds
        )
    }

    val sums = rows.reduce((a, b) => (
      a._1 + b._1,
      a._2 + b._2,
      a._3 + b._3
//      a._4 + b._4,
//      a._5 + b._5,
//      a._6 + b._6 //,
      //    a._7 + b._7,
      //    a._8 + b._8,
      //    a._9 + b._9
    )
    )
    val avg = List[Any](
      sums._1 / count,
      sums._2 / count,
      sums._3 / count
//      sums._4 / count,
//      sums._5 / count,
//      sums._6 / count
      //    sums._7 / count,
      //    sums._8 / count,
      //    sums._9// / count
    ).mkString(",")

    println(avg)
  }

}

object LNCAsModel_Comparison2 extends App with AppConfig {

  val n = 20
  val size = 20

  val vars = (1 to n).map(i => Var(s"p$i")).toList

  val count = 10
  for (d <-0 to 3) {
    val rows = LNCFormulaGenerator.generateRandomFormulas(count, vars, d, size).zipWithIndex.map {
      case (formula, idx) =>
        val (model1, time1) = time.measureTime {
          LNCToKripkeStructureConverter.convertFull(formula, SatSolvers.tableAux)
        }

        (model1.nodes.size, model1.edges.values.map(_.size).sum, time1.seconds)
    }

    val sums = rows.reduce((a, b) => (
      a._1 + b._1,
      a._2 + b._2,
      a._3 + b._3
    ))
    val avg = List[Any](
      sums._1 / count,
      sums._2 / count,
      sums._3 / count
    ).mkString(",")

    println(avg)
  }

}