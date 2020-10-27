
object Zombies extends App {
  def countBad(hs: List[Int]): Int = {
    def mergeCount(left: (Int,List[Int]), right: (Int,List[Int]), ans :Int, merged: List[Int]): (Int,List[Int]) =
      (left._2, right._2) match {
        case (_, Nil) => (ans,merged:::left._2)
        case (Nil, _) => (ans,merged:::right._2)
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) {
            val newAns = left._1 + right._1 + left._2.length
            println(newAns,merged)
            mergeCount((0,left._2), (0,rightTail), ans+newAns, merged:::List(rightHead))
          } else mergeCount((left._1,leftTail), right, ans, merged:::List(leftHead))
      }
    def mergeSortPair(list: List[Int]): (Int,List[Int]) ={
      val n = list.length / 2
      if (n == 0) (0, list)
      else {
        val (left, right) = list.splitAt(n)
        mergeCount(mergeSortPair(left), mergeSortPair(right), 0, List())
      }
    }
    val result = mergeSortPair(hs)
    result._1
  }
}