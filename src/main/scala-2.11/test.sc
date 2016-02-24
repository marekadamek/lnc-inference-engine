import pl.edu.pw.elka.madamek.nclogic.model.Types.{Var, And}
val s1 = And(Set(Var("a"), Var("b")))
val s2 = And(Set(Var("b"), Var("c")))
s1 == s2