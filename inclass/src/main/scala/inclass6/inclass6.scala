package inclass6


package inclass6

object Polymorph extends App {

  trait Dessert

  case class Pie(kind: String) extends Dessert

  case class Smoothie(fruits: List[String]) extends Dessert

  case class Cake(toppings: String) extends Dessert

  val intL1 = List(1, 3, 5, 6, 7, 8) //List[(Int,Int)]
  val strL1 = List('a', 'b', 'c') //List[(Int,Int)]
  val intL2 = List(33, 24, 55, 66, 77, 28) //List[(Int,Int)]
  val intL3 = List(33, 24, 55, 66, 77) //List[(Int,Int)]
  val intL4 = List(1, 1, 1, 1, 3, 3, 3, 5, 6, 7, 8) //List[(Int,Int)]
  val L = List((1, 1), (4, 1), (5, 5), (6, 5)) //List[(Int,Int)]

  def unzip[A, B](xs: List[(A, B)]): (List[A], List[B]) = {
    def rev[A, B](x: List[(A, B)], revx: List[(A, B)]): List[(A, B)] = {
      x match {
        case Nil => revx
        case head :: tail => rev(tail, head :: revx)
      }
    }

    def helper[A, B](x: List[(A, B)],
                     z1: List[A],
                     z2: List[B]): (List[A], List[B]) = {
      x match {
        case head :: tail => helper(tail, head._1 :: z1, head._2 :: z2)
        case Nil => (z1, z2)
      }
    }

    helper(rev(xs, List()), List(), List())
  }

  def countWhile[T](xs: List[T], key: T): Int = {
    def helper[T](x: List[T], count: Int): Int = {
      //      println(x, key, count)
      x match {
        case head :: tail => {
          if (head == key) {
            helper(tail, count + 1)
          } else {
            helper(tail, count)
          }
        }
        case Nil => count
      }
    }

    helper[T](xs, 0)
  }

  def topK(xs: List[Int], k: Int): List[Int] = {
    // count and sort the frequencies
    // recursive merge of 2 sorted lists
    def merge(left: List[Int], right: List[Int]): List[Int] =
      (left, right) match {
        case (left, Nil) => left
        case (Nil, right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) leftHead :: merge(leftTail, right)
          else rightHead :: merge(left, rightTail)
      }

    def mergeSort(list: List[Int]): List[Int] = {
      val n = list.length / 2
      if (n == 0)
        list // i.e. if list is empty or single value, no sorting needed
      else {
        val (left, right) = list.splitAt(n)
        merge(mergeSort(left), mergeSort(right))
      }
    }

    def countMap(x: List[Int],
                 count: Int,
                 key: Int,
                 map: List[(Int, Int)]): List[(Int, Int)] = {
      //      println(x, key, count)
      x match {
        case head :: tail => {
          if (head == key) {
            countMap(tail, count + 1, head, map)
          } else {
            countMap(tail, 1, head, (key, count) :: map)
          }
        }
        case Nil => map
      }
    }

    def mergePair(left: List[(Int, Int)],
                  right: List[(Int, Int)]): List[(Int, Int)] =
      (left, right) match {
        case (left, Nil) => left
        case (Nil, right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead._2 < rightHead._2) leftHead :: mergePair(leftTail, right)
          else rightHead :: mergePair(left, rightTail)
      }

    def mergePairSort(list: List[(Int, Int)]): List[(Int, Int)] = {
      val n = list.length / 2
      if (n == 0)
        list // i.e. if list is empty or single value, no sorting needed
      else {
        val (left, right) = list.splitAt(n)
        mergePair(mergePairSort(left), mergePairSort(right))
      }
    }

    def findTopK(mapped: List[(Int, Int)]): List[Int] = {
      def helper(ex: List[(Int, Int)],
                 n: Int,
                 ans: List[(Int, Int)]): List[(Int, Int)] = {
        ex match {
          case Nil => ans
          case head :: tail =>
            if (n > 0) {
              helper(tail, n - 1, head :: ans)
            } else {
              ex
            }
        }
      }

      unzip[Int, Int](helper(mapped, mapped.length - k, List()))._1
    }

    findTopK(
      mergePairSort(countMap(mergeSort(xs), 0, Int.MinValue, List())).tail)
  }

  def isLiquid(what: Dessert): Boolean = {
    what match {
      case Smoothie(fruits) => true
      case _ => false
    }
  }

  println(unzip[Int, Int](L))
  println(countWhile[Char](strL1, 'a'))
  println(topK(intL4, 2))
  println(isLiquid(Cake("Chocolate")))
  println(isLiquid(Smoothie(List("Banana"))))
}