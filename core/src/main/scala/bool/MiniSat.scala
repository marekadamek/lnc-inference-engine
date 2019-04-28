package bool

import java.nio.file.Files

import nclogic.AppConfig
import nclogic.model.expr.Expr

import scala.collection.JavaConverters._
import scala.sys.process.{ProcessLogger, _}

trait MiniSat extends CnfSat with AppConfig {
  private val miniSatPath = config.getString("minisatPath")


  def solveSAT(cnf: String): Option[List[Int]] = {
    val in = Files.createTempFile("minisatInput", "tpm")
    val out = Files.createTempFile("minisatOut", "tmp")

    Files.write(in, cnf.getBytes("UTF-8"))

    s"$miniSatPath $in $out" lines_! ProcessLogger(_ => ())

    val outPutFile = Files.readAllLines(out).asScala

    val result = outPutFile.head match {
      case "SAT" =>
        val solution = outPutFile(1).split(" ").map(_.toInt)
        Some(solution.toList)
      case "UNSAT" =>
        None
      case _ => throw new Exception("Unexpected Minisat output")
    }

    Files.deleteIfExists(in)
    Files.deleteIfExists(out)

    result
  }

}
