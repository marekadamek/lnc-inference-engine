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

  val n = 50

  val formatter = new SimpleDateFormat("hh:mm")

  //for (d <- 100 to 1000 by 100) {
  for (d <- 1 to 16 by 1) {
    //println()
    //println(d + " " + formatter.format(new Date))
    //println()

    // begin table
    val (formula, fileName) = TestFormulas.negativeC(n, d)

//    val (prefixFormula2, prefixTime2) = measureTime {
//      PrefixFormulaConverter.convert2(formula)
//    }

    // begin table
//    val (solution, tableTime) = measureTime {
//      prefixFormula2 match {
//        case False => None
//        case True => Some(True)
//        case base =>
//          TableAuxFinal(base).next()
//      }
//    }
//
   // println(s"${prefixTime2.seconds}, ${prefixFormula2}")
//    println(s"${prefixTime2.seconds}, ${tableTime.seconds}")
    // end table



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

        //trp++
//        val trpDir = config.getString("trpFilesDir")
//        val (_, trpTime) = measureTime {
//          TrpExporter.convert(formula, Paths.get(trpDir, s"$fileName.trp"))
//        }
//        println(s"TRP++ export time (ms): ${trpTime.seconds}")

    ////    //aalta
        val aaltaDir = config.getString("aaltaFilesDir")
        val (_, aaltaTime) = measureTime {
          AaltaExporter.convert(formula, Paths.get(aaltaDir, s"$fileName.aalta"))
        }
    //
        //NuSMV
//        val nuSmvDir = config.getString("nusmvFilesDir")
//        val (_, smvTime) = measureTime {
//          NuSmvExporter.convert(formula, Paths.get(nuSmvDir, s"$fileName.smv"))
//        }
//        println(s"NuSmv export time (ms): ${smvTime.seconds}")

    //}
  }
}