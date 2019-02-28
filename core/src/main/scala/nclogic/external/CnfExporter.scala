package nclogic.external

import java.io.{ByteArrayInputStream, File}
import java.nio.file.{Files, Path}

import nclogic.external.BoolToCNF.config
import nclogic.model.expr.{Expr, Next, Var}

import scala.sys.process._
import scala.io.Source

object CnfExporter {

  private val boolToCNF = config.getString("boolToCnfPath")

  private def exportToFile(formula: String, outputPath: Path): Map[Int, String] = {
    val is = new ByteArrayInputStream(formula.getBytes("UTF-8"))
    val mapFile = Files.createTempFile("cnfMap", "tmp")


    (s"$boolToCNF $mapFile" #< is #> new File(outputPath.toString)).!

    val mapFileSource = Source.fromFile(mapFile.toFile)

    val variablesMap = mapFileSource.getLines.map(l => {
      val splited = l.split(" ")
      splited(1).toInt -> splited(0)
    }).toMap

    variablesMap
  }

  def exportToFile(e: Expr, outputPath: Path): Map[Int, Expr] = {
    val variableMap = exportToFile(BoolExporter.convert(e), outputPath)

    val exprMap = variableMap.mapValues { v =>
      if (v.startsWith("X")) {
        val parts = v.split("_")
        Next(Var(parts(2)), parts(1).toInt)
      } else {
        Var(v)
      }
    }

    exprMap
  }
}
