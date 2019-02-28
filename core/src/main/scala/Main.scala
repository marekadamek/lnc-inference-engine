import java.nio.file.Paths

import nclogic.external.PltlExporter
import nclogic.model.LNC
import nclogic.sat.TableAux

object Main extends App {

  import nclogic.model.expr._




  val n = 1
  val d = 1

  val formula = {
    val base = Or((for (i<- 1 to n) yield Var(s"p$i").asInstanceOf[Expr]).toSet)
    C(d, base)
  }
  val pf = LNC.prefixFormula(formula)

  val fileName = s"test.ltl"
  val path = Paths.get(s"/Users/marek/phd/files/pltl/$fileName")
  PltlExporter.convert(pf, path)


  println("ok")
  //println(solver.isSatisfiable(pf))
  println(TableAux.isSatisfiable(pf))



  //Files.write(Paths.get(s"/Users/marek/phd/files/bool/test.bool"), BoolExporter.convert(pf).getBytes("UTF-8"))
  //println("lk")
  //println(solver.isSatisfiable(f))
  //println(solver.getAllBaseFormulaSolutions(f))

//  for (i <- 1 to 50) {
//    println(i)
//    val f = LNC.prefixFormula(C(a, i))
//    val fileName = s"C${i}a.bool"
//    val path = Paths.get(s"/Users/marek/phd/files/mettel/$fileName")
//
//    MettelExporter.convert(f, path)
//  }
}