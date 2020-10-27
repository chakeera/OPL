trait betterExpr {
  //Declaring the functions
  def +(that: betterExpr) = Sum(this, that)
  def *(that: betterExpr) = Prod(this,that)
  def unary_- = Negate(this)
  def -(that: betterExpr) = Sub(this, that)
  def /(that: betterExpr) = Div(this, that)
  def ~ = LogNeg(this)
  def toVal(implicit ctx: Map[String, Double]): Double
}

case class Var(name: String) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = ctx.getOrElse(name, 0)
}
case class Constant(n: Double) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = n
}
case class Sum(e1: betterExpr, e2: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e1.toVal + e2.toVal
}
case class Prod(e1: betterExpr, e2: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e1.toVal * e2.toVal
}
case class Negate(e: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e.toVal * -1
}
case class Sub(e1: betterExpr, e2: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double = e1.toVal - e2.toVal
}
case class Div(e1: betterExpr, e2: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    try
      {//check if the denom is zero then throw exception and return e1/e2
        if(e2.toVal == 0){throw new ArithmeticException()}
        e1.toVal/e2.toVal
      }
      // Catch clause and Case statement
    catch
      {
        case x: ArithmeticException =>
        {
          println("Exception: A number is not divisible by zero.")
          0.0 // needs to return a double so I put 0.0 (quite a brute force debugging but no idea what to do about it)
        }
      }
}
case class LogNeg(e1: betterExpr) extends betterExpr {
  override def toVal(implicit ctx: Map[String, Double]): Double ={
//    We are doing two's complement here
    if (e1.toVal == e1.toVal.toInt){
      (-1*(e1.toVal+1)) // Thanks to my digital logic lecture notes and stack overflow
    }
    else{
  // convert the input to binary using Java lib and put it in the list so that we can loop over and flip each number
  val convert2binary= java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(e1.toVal)).toList
    // using tail recursion to loop each string and then flip them
      def fliphelper(S:List[Char], a:List[Char] ):List[Char]= S match {
        case Nil => a
        case x :: xs =>
          if (x == '0') {
            fliphelper(xs, '1' :: a)
          }
          else {
            fliphelper(xs, '0' :: a)
          }
      }
    //get the flipped binary
    val getFlipped = fliphelper(convert2binary, List()).mkString("") // thank you stack over flow
    val LogNegAns = java.lang.Long.parseLong(getFlipped,2).doubleValue
    LogNegAns
    }
  }
}


//Test Case
object Main extends App{
  //Test case for +,*
//val x = Var("x")
//val ex = (x + Constant(5))*x + Constant(11)*x
//implicit val ctx = Map("x" -> 2.0)
//println(ex.toVal)  // Here it should return 36


//  Test Case for -,/
//  val y = Var("y")
//  val ey = (y - Constant(5))/y - Constant(10)/y //10-5/10 = 1/2 - 2= -3/2
//  implicit val cty = Map("y" -> 10)
//  println(ey.toVal)
//  val x = Var("x")
//  val ex = (Constant(10)/x)
//  implicit val ctx =  Map("x" -> 0.0)
//  println(ex.toVal)

//  Test Case for log neg and negate ~
//    val x = Var("x")
//    implicit val ctx =  Map("x" -> 4.5)
//    println(x.~.toVal)
//      val x = Var("x")
//      implicit val ctx =  Map("x" -> 4.0)
//      println(x.~.toVal)


}



