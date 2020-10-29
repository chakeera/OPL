package Assignment2

object FilterMap {

  object FilterMap extends App {
    def map[A, B](f: A => B, xs: List[A]): List[B] = {
      def helperfunc(f: A => B, xs: List[A], newxs: List[B]): List[B] = xs match {
        case Nil => newxs
        case head :: tail => helperfunc(f, tail, newxs :::List(f(head)))
      }
      helperfunc(f, xs, List())
    }

    def filter[A](p: A => Boolean, xs: List[A]): List[A] = {
      def helperfunc(p: A => Boolean, xs: List[A], newxs: List[A]): List[A] = xs match {
          case Nil => newxs
          case head :: tail if p(head) => helperfunc(p, tail, newxs :::List(head))
          case _ :: tail => helperfunc(p, tail, newxs)
        }
      helperfunc(p, xs, List())
    }
  }
}
