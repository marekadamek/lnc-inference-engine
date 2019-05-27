package latex

import scala.io.Source

object CSVtoTable extends App {

  val csv = "/Users/marek/git/phd/csv/mc/random_3.csv"

  val it = Source.fromFile(csv).getLines()

  val header = it.next().split(",").length

  val lines = it.map(line => {
    var splitted = line.split(",").toList
    splitted = splitted ++ (1 to header - splitted.length).map(_ => "").toList
    splitted.map {
      case x if x.isEmpty => "---"
      case x => "$ " + x + " $"
    }.mkString(" & ")
  })

  val output = lines.mkString(" \\\\ \n\\hline\n") + " \\\\ \n\\hline"

  println(output)
}
