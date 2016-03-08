import pl.edu.pw.elka.madamek.nclogic.model.Types.{Var, And}

implicit class Iff[T](val b: T) {
  def :>[B](f: T => B):B = f(b)
}

val f = (x:Int) => x*2
val g = (x:Int) => x-2
val h = (x:Int) => Math.sqrt(x)

h(g(f(5))) == 5 :> f :> g :> h