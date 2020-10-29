package Assignment2

object ReadAloud extends App {

    def readAloud(lst: List[Int]): List[Int] = {

      def helperfunc(lst: List[Int], count: Int, newlst: List[Int], current: Int): List[Int] = lst match {
        case Nil => newlst :+ count :+ current
        case x :: xs if (x == current) => helperfunc(xs, count + 1, newlst, current)
        case x :: xs => helperfunc(xs, 1,newlst :+ count :+ current, x)
      }
      helperfunc(lst, 0,List(), lst.head)
    }
    def unreadAloud(rlst: List[Int]): List[Int] = {

      def helperfunc2(lst: List[Int], newlst: List[Int], count: Int, current: Int): List[Int] = count match {
        case 0 => lst match {
          case Nil => newlst
            case x :: xs => helperfunc2(xs.tail, newlst, x, xs.head)
          }
        case _ => helperfunc2(lst, newlst :+ current, count - 1, current)
      }
      helperfunc2(rlst.tail.tail, List(), rlst.head, rlst.tail.head)
    }
}
