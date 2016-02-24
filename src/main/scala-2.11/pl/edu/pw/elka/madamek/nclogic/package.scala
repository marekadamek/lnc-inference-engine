package pl.edu.pw.elka.madamek

import pl.edu.pw.elka.madamek.nclogic.model.Types.Expr
import pl.edu.pw.elka.madamek.nclogic.solver.Cnf

package object nclogic {

  implicit def toCnf(expr: Expr): Cnf = Cnf(expr)

}
