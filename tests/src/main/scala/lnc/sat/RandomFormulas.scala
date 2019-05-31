package lnc.sat

import java.nio.file.Paths

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.ltl.Always
import lnc.external.{NuSmvExporter, PltlExporter}
import lnc.mc.{LNCFormulaGenerator, LNCModel}
import lnc.{AppConfig, LNC}
import time._


object RandomFormulas extends App with AppConfig {

  val n = 10
  val size = 30
  val vars = (1 to n).map(i => Var(s"p$i")).toList

  for (d <- 10 to 10) {
    val formulas = LNCFormulaGenerator.generateRandomFormulas(100, vars, d, size)
      .map(NormalFormConverter.preprocess)

    //cycle
    val (_, cycleTime) = measureTime {
      formulas.map(LNCModel.findCycle(_, SatSolvers.tableAux))
    }
//
//    val (prefixes, prefixTime) = measureTime {
//      formulas.map(LNC.calculatePrefixFormula)
//    }
//
//    val (_, tableAuxTime) = measureTime {
//      prefixes.map {
//        case False => None
//        case True => Some(True)
//        case prefixFormula =>
//          val normal = NormalFormConverter.convertToNormalForm(prefixFormula)
//          SatSolvers.tableAux.iterator(normal).next()
//      }
//    }

//    println(List[Any](n, d, cycleTime.seconds, prefixTime.seconds, tableAuxTime.seconds).mkString(","))
    println(List[Any](n, d, cycleTime.seconds).mkString(","))

//    //ptlt
//    val filename = s"random_${n}_$d.pltl"
//    PltlExporter.convert(formulas.map(Always), Paths.get("/Users/marek/phd/files/pltl/", filename))
//
//    //smv
//    NuSmvExporter.convert(formulas, Paths.get("/Users/marek/phd/files/nusmv/", filename))
  }
}