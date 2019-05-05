package lnc.external

import java.io.ByteArrayInputStream
import java.nio.file.Files

import lnc.AppConfig
import lnc.expr._
import scala.collection.JavaConverters._
import scala.sys.process._

object BoolToCNF extends App with AppConfig {
  lazy val boolToCNF = config.getString("boolToCnfPath")

  def convert(formula: String): (String, Map[Int, String]) = {
    val is = new ByteArrayInputStream(formula.getBytes("UTF-8"))
    val mapFile = Files.createTempFile("cnfMap", "tmp")

    val cnf = (s"$boolToCNF $mapFile" #< is).!!

    val variablesMap = Files.readAllLines(mapFile).asScala.map(l => {
        val splited = l.split(" ")
        splited(1).toInt -> splited(0)
      })
      .toMap

    (cnf, variablesMap)
  }

  def convert(e: Expr): (String, Map[Int, Expr]) = {
    val (cnf, variableMap) = convert(BoolExporter.convert(e))

    val exprMap = variableMap.mapValues { v =>
      if (v.startsWith("X")) {
        val parts = v.split("_")
        Next(Var(parts(2)), parts(1).toInt)
      } else {
        Var(v)
      }
    }

    (cnf, exprMap)
  }
}
