package nclogic.model.expr

class SubstitutionSet() {
  private var bindings: Map[Var, Unifiable] = Map.empty

  def this(s: SubstitutionSet) {
    this
    bindings = bindings ++ s.bindings
  }

  def clear(): Unit = bindings = Map.empty

  def add(v: Var, exp: Unifiable): Unit = {
    bindings = bindings + Tuple2(v, exp)
  }

  def getBinding(v: Var): Option[Unifiable] = bindings.get(v)

  override def toString: String = "Bindings:[" + bindings + "]"
}
