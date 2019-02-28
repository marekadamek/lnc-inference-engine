package bool

import java.io.ByteArrayInputStream

import nclogic.AppConfig

import scala.sys.process.{ProcessLogger, _}

trait LingeringSat extends CnfSat with AppConfig {
  private val solverPath = config.getString("lingeringPath")


  def solveSAT(cnf: String): Option[List[Int]] = {
    val in = new ByteArrayInputStream(cnf.getBytes("UTF-8"))

    var result = Option.empty[List[Int]]
    solverPath #< in ! ProcessLogger((x) => {
      if (x.charAt(0) == 'v') {
        result = Some(x.substring(2).split(" ").map(_.toInt).toList)
      }
    })

    result
  }
}

