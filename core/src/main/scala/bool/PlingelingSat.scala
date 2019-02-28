package bool

import java.io.ByteArrayInputStream

import nclogic.AppConfig

import scala.sys.process.{ProcessLogger, _}

trait PlingelingSat extends CnfSat with AppConfig {
  private val solverPath = config.getString("plingelingPath")


  def solveSAT(cnf: String): Option[List[Int]] = {
    val in = new ByteArrayInputStream(cnf.getBytes("UTF-8"))

    var lines = List.empty[String]
    solverPath #< in ! ProcessLogger((x) => {
      if (x.charAt(0) == 'v') {
        lines = x.substring(2) :: lines
      }
    })

    lines.reverse match {
      case Nil => None
      case _ =>  Some(lines.mkString(" ").split(" ").map(_.toInt).toList)
    }

  }
}

