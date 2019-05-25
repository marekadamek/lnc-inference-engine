package latex

import scala.collection.mutable.ListBuffer
import scala.io.Source

object CSVtoPlots extends App{

  val csv = "/Users/marek/git/phd/csv/c_50.csv"

  val it = Source.fromFile(csv).getLines()

  val header = it.next()

  //private def normalize(v: Float) = BigDecimal(v).setScale(3, BigDecimal.RoundingMode.HALF_UP).toFloat


  header.split(",").toList match {
    case _ :: plots =>

      val plotMap = plots.zipWithIndex.map(p => p._2 -> new StringBuilder).toMap

      it.drop(8).foreach(line => {
        line.split(",").toList match {
          case d :: times =>
            times.zipWithIndex.filter(_._1.nonEmpty).foreach(x => plotMap(x._2).append(s"($d, ${x._1})"))
        }
      })

      val latexPlots = plotMap.toList.sortBy(_._1).map(_._2).map(lb => {
        s"""
           |\\addplot
           |coordinates{${lb.toString}};
         """.stripMargin
      })

      val legend =
        s"""
          |\\legend{
          | ${plots.map(p => s"\\textit{$p}").mkString(", ")}
          |}
        """.stripMargin

     val output = (latexPlots ++ List(legend)).mkString("\n")

      println(output)
  }
}
