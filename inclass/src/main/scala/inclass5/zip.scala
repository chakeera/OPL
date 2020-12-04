package inclass5


object Zip extends App{
  var intL1 = List(1,3,5,6,7,8) //List[(Int,Int)]
  var intL2 = List(33,24,55,66,77,28) //List[(Int,Int)]
  var intL3 = List(33,24,55,66,77) //List[(Int,Int)]
  var L = List((1, 1), (4, 1), (5, 5), (6, 5)) //List[(Int,Int)]

  def zip(x : List[Int], y: List[Int]) : List[(Int, Int)] = {
    def rev(x : List[(Int, Int)], revx : List[(Int, Int)]): List[(Int, Int)] = {
      x match {
        case Nil => revx
        case head::tail => rev(tail, head::revx)
      }
    }
    def helper(x : List[Int], y: List[Int], z: List[(Int, Int)]) : List[(Int, Int)] = {
      (x,y) match {
        case (head::tail, head2::tail2) => helper(tail, tail2, (head,head2)::z)
        case (head::tail, Nil) => Nil
        case (Nil, head::tail) => Nil
        case (Nil,Nil) => z
      }
    }
    rev(helper(x,y,List()), List())
  }

  def unzip(zipped : List[(Int, Int)]) : (List[Int], List[Int])  = {
    def rev(x : List[(Int, Int)], revx : List[(Int, Int)]): List[(Int, Int)] = {
      x match {
        case Nil => revx
        case head::tail => rev(tail, head::revx)
      }
    }
    def helper(x : List[(Int, Int)], z1: List[Int], z2: List[Int]) : (List[Int], List[Int]) = {
      x match {
        case head::tail => helper(tail, head._1::z1,head._2::z2)
        case Nil => (z1,z2)
      }
    }
    helper(rev(zipped,List()), List(),List())
  }

//  println(zip(intL1,intL2))
//  println(zip(intL1,intL3))
//  println(unzip(L))
}