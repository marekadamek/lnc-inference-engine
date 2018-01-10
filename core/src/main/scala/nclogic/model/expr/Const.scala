package nclogic.model.expr


trait Const extends Expr {
  val isAtomic = true
  val simplify = this
}

case object True extends Const {
  override def toString = "T"
}

case object False extends Const {
  override def toString = "F"
}