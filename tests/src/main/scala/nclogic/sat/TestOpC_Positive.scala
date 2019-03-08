package nclogic.sat

import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date

import nclogic.AppConfig
import nclogic.external._
import nclogic.model.expr.{False, True, Var}
import nclogic.model.{LNC, NormalFormConverter, PrefixFormulaConverter}
import time._

object TestOpC_Positive extends App with AppConfig {

  val n = 10

  val formatter = new SimpleDateFormat("hh:mm")

  //for (d <1 1z to 40000 by 2000) {
  for (d <- 1 to 16 by 1) {
//    println()
    println(d + " " + formatter.format(new Date))
//    println()

    val (formula, fileName) = TestFormulas.negativeN(n, d)

    val solution = PrefixFormulaConverter.getCommonBase(formula) match {
      case False => None
      case True => TableAuxFinal(NormalFormConverter.convertToLN(formula)).next()
      case base =>
        val baseDnf = TableAuxFinal(base).solveAll
        TableAuxFinal(NormalFormConverter.convertToLN(formula), baseDnf).next()
    }

    //println(NormalFormConverter.convertToLN(formula))
     println(s"Formula: $solution")

    //prefix

//    val (prefixFormula2, prefixTime2) = measureTime {
//      PrefixFormulaConverter.convert2(formula)
//    }
//
    //println(prefixFormula2)
   // println("PREFIX2: " + prefixTime2.seconds)

   // assert(false)

//    //println(s"Prefix formula calculation time (s): ${prefixTime.seconds}, ${LNC.depth(prefixFormula)}")
//
//    //semantic table
//
//    val (result, semanticTableTime) = measureTime {
//      TableAuxBDD2.solveOne(prefixFormula2)
//    }
////
//    println(result.isDefined)
    //println(prefixTime.seconds)
//
//    //cnf
//        val cnfDir = config.getString("cnfFilesDir")
//        val (map, cnfTime) = measureTime {
//          val forSAT = LNC.simplify(NormalFormConverter.moveNInside(prefixFormula2))
//          CnfExporter.exportToFile(forSAT, Paths.get(cnfDir, s"$fileName.cnf"))
//        }
//        println(cnfTime.seconds)

    //
    //    //pltl
//    val pltlDir = config.getString("pltlFilesDir")
//    val (_, pltlTime) = measureTime {
//      PltlExporter.convert(NormalFormConverter.convertToLN(formula), Paths.get(pltlDir, s"$fileName.pltl"))
//    }
    //println(s"PLTL export time (ms): ${pltlTime.seconds}")

    //pltl

    //    //trp++
    //    val trpDir = config.getString("trpFilesDir")
    //    val (_, trpTime) = measureTime {
    //      TrpExporter.convert(formula, Paths.get(trpDir, s"$fileName.trp"))
    //    }
    //    println(s"TRP++ export time (ms): ${trpTime.seconds}")
    //
    ////    //aalta
    //    val aaltaDir = config.getString("aaltaFilesDir")
    //    val (_, aaltaTime) = measureTime {
    //      AaltaExporter.convert(formula, Paths.get(aaltaDir, s"$fileName.aalta"))
    //    }
    //
    //    //NuSMV
    //    val nuSmvDir = config.getString("nusmvFilesDir")
    //    val (_, smvTime) = measureTime {
    //      NuSmvExporter.convert(formula, Paths.get(nuSmvDir, s"$fileName.smv"))
    //    }
    //    println(s"NuSmv export time (ms): ${smvTime.seconds}")

    //}
  }
}