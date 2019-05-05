package lnc.sat

import java.nio.file.Files

import lnc.AppConfig

import scala.collection.JavaConverters._
import scala.sys.process.{ProcessLogger, _}

trait GlucoseSat extends CnfSat with AppConfig {
  private val solverPath = config.getString("glucosePath")


  def solveSAT(cnf: String): Option[List[Int]] = {
    val in = Files.createTempFile("glucoseInput", "tpm")
    val out = Files.createTempFile("glucoseOut", "tmp")

    Files.write(in, cnf.getBytes("UTF-8"))

    s"$solverPath $in $out" lines_! ProcessLogger(_ => ())

    val outPutFile = Files.readAllLines(out).asScala

    val result = outPutFile.head match {
      case "UNSAT" =>
        None
      case line =>
        val solution = line.split(" ").map(_.toInt)
        Some(solution.toList)
    }

    Files.deleteIfExists(in)
    Files.deleteIfExists(out)

    result
  }

}

