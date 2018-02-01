//package nclogic.lang
//
//import nclogic.model.expr.Expr
//
//class Block(block: => Unit) extends Expr {
//  override def simplify: Expr = this
//
//  override def isAtomic: Boolean = true
//
//  def execute(): Unit = block
//}
