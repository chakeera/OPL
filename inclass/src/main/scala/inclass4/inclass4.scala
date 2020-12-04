package inclass4

object Inclass4 extends App {
  var L = List((1, 1), (4, 1), (5, 5), (6, 5)) //List[(Int,Int)]
  var LS = List((1, "abc"), (4, "qwer"), (5, "iop"), (6, "kkk")) //List[(Int,Int)]
  var intL = List(1,3,5,6,7,8) //List[(Int,Int)]
  var i = 0
  def find(xs: List[(Int, String)], key: Int):
  Option[String] = {
    def helperfunc(xs: List[(Int, String)], value: String): Option[String] = {
      xs match {
        case Nil => None
        case head :: tail => if(head._1 == key) Some(head._2) else helperfunc(tail, value)
      }
    }
    helperfunc(xs, null)
  }
  def rev(xs: List[Int]): List[Int] = {
    def helperfunc(xs: List[Int], revert: List[Int]): List[Int] = {
      xs match {
        case Nil => revert
        case head :: tail => helperfunc(tail, head::revert)
      }
    }

    helperfunc(xs, List())
  }

  def fib(n: Int): Long = {
    def helperfunc(n: Int, n1: Long, n2: Long): Long = {
      n match {
        case 0 => n1
        case 1 => n2
        case _ => helperfunc(n-1, n2, n1+n2)
      }
    }

    helperfunc(n, 0, 1)
  }
  println(find(LS,1))
  println(find(LS,666))
  println(rev(intL))
  for(i <- 0 to 10){
    print(fib(i).toString+",")
  }
  print(fib(11))

}