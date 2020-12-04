package inclass3

object inclass3 extends App {
  def sumPairList(xs: List[(Int, Int)]): Int = {
    def helper(xs: List[(Int, Int)], sum: Int): Int = {
      xs match {
        case Nil => sum
        case head :: tail => helper(tail, head._1 + head._2 + sum)
      }
    }

    helper(xs, 0)
  }

  def firsts(xs: List[(Int, Int)]): List[Int] = {
    def helper(xs: List[(Int, Int)], merged: List[Int]): List[Int] = {
      xs match {
        case Nil => merged
        case head :: tail => helper(tail, head._1 :: merged)
      }
    }

    helper(xs, List())
  }

  def seconds(xs: List[(Int, Int)]): List[Int] = {
    def helper(xs: List[(Int, Int)], merged: List[Int]): List[Int] = {
      xs match {
        case Nil => merged
        case head :: tail => helper(tail, head._2 :: merged)
      }
    }

    helper(xs, List())
  }

  def pairSumList(xs: List[(Int, Int)]): (Int, Int) = {
    def helper(xs: List[(Int, Int)], sumPair: (Int, Int)): (Int, Int) = {
      xs match {
        case Nil => sumPair
        case head :: tail => helper(tail, (sumPair._1 + head._1, sumPair._2 + head._2))
      }
    }

    helper(xs, (0, 0))
  }
}
