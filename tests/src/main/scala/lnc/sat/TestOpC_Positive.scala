package lnc.sat

import java.text.SimpleDateFormat

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.mc.LNCModel
import lnc.{AppConfig, LNC}
import time._

object TestOpC_Positive extends App with AppConfig {

  val n = 100

  val formatter = new SimpleDateFormat("hh:mm")

  //for (d <- 100 to 1000 by 100) {
  for (d <- 100 to 1000 by 100) {
    //println()
    //println(d + " " + formatter.format(new Date))
    //println()

    // begin table
    val (formula, fileName) = {
      val (f, file) = TestFormulas.positiveN(n, d)
      val ln = NormalFormConverter.convertToLN(f)
      val optimized = NormalFormConverter.preprocess(ln)
      (optimized, file)
    }

//    val (cycle, cycleTIme) = measureTime {
//      //TableAux(formula).next()
//      LNCModel.findCycle(formula, SatSolvers.tableAux)
//    }
//    println(d, cycleTIme.seconds, cycle.isDefined)


    val (prefixFormula, prefixTime) = measureTime {
      LNC.calculatePrefixFormula(formula)
    }


    val (solution, tableTime) = measureTime {
      prefixFormula match {
        case False => None
        case True => Some(True)
        case _ =>
          val normal = NormalFormConverter.convertToNormalForm(prefixFormula)
          SatSolvers.tableAux.iterator(normal).next()
      }
    }

    println(d, prefixTime.seconds, tableTime.seconds, solution.isDefined)


    //    println(s"$d ${prefixTime.seconds}, ${tableTime.seconds}")
    //    println(s"${solution.isDefined}")

    //    val (normal, normalTime) = measureTime {
    //      NormalFormConverter.convertToNormalForm(formula)
    //    }

    //    val (solution2, tableTime2) = measureTime {
    //      TableAuxBDDLNC(normal).next()
    //    }
    //
    //    println(s"$d ${normalTime.seconds}, ${tableTime2.seconds}")
    //    println(s"${solution2.isDefined}")


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
    //      //PltlExporter.convert(prefixFormula, Paths.get(pltlDir, s"$fileName.pltl"))
    //      PltlExporter.convert(prefixFormula, Paths.get(pltlDir, s"test$d.pltl"))
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
    //        val aaltaDir = config.getString("aaltaFilesDir")
    //        val (_, aaltaTime) = measureTime {
    //          AaltaExporter.convert(formula, Paths.get(aaltaDir, s"$fileName.aalta"))
    //        }
    //    //
    //NuSMV
    //        val nuSmvDir = config.getString("nusmvFilesDir")
    //        val (_, smvTime) = measureTime {
    //          NuSmvExporter.convert(formula, Paths.get(nuSmvDir, s"$fileName.smv"))
    //        }
    //        println(s"NuSmv export time (ms): ${smvTime.seconds}")

    //}
  }
}