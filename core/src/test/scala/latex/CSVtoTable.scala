package latex

import scala.io.Source

object CSVtoTable extends App {

  val csv = "/Users/marek/git/phd/csv/not_n100.csv"

  val it = Source.fromFile(csv).getLines()

  val header = it.next()

  val lines = it.map(line => {
    line.split(",").map {
      case x if x.isEmpty => "---"
      case x => "$ " + x + " $"
    }.mkString(" & ")
  })

  val output = lines.mkString(" \\\\ \n\\hline\n") + " \\\\ \n\\hline"

  println(output)
}
