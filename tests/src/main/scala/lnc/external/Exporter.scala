package lnc.external

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}
import java.nio.file.Path

import lnc.expr.Expr

import scala.util.Try

trait Exporter {

  def convert(input: List[Expr]): String = {
    val sw = new StringWriter()
    Try(convert(input, sw))
    sw.close()
    sw.toString
  }

  def convert(input: List[Expr], path: Path): Unit = {
    val writer = new BufferedWriter(new FileWriter(path.toFile))
    Try(convert(input, writer))
    writer.close()
  }

  def convert(input: List[Expr], writer: Writer): Unit

}
