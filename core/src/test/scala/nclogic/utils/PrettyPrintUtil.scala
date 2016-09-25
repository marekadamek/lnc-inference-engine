package nclogic.utils

import nclogic.model.DnfConverter
import nclogic.model.Types.{Var, Neg}

object PrettyPrintUtil {

  //def printPath(path: List[DnfConverter.AndClause]): Unit = path.foreach(and => println(and.mkString(" & ")))

  def printPath(path: List[DnfConverter.AndClause]): Unit = {
    path.foreach(and => println(and
        .filterNot(_.isInstanceOf[Neg])
        .toList
        .sortBy({
          case Neg(Var(name)) => name
          case Var(name) => name
        })
        .mkString(" & ")))
  }
}
