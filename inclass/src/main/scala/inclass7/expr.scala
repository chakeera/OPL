package inclass7

trait Expr {
  def +(that: Expr) = Sum(this,that)
  def *(that: Expr) = Prod(this,that)
  def unary_- = Negate(this)
  def toVal(implicit ctx: Map[String, Double]): Double
}
case class Var(name: String) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = if(ctx.contains(name)) ctx.get(name).get else 0
}
case class Constant(n: Double) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = n
}
case class Negate(e: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e.toVal * -1
}
case class Sum(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e1.toVal + e2.toVal
}
case class Prod(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e1.toVal * e2.toVal
}