object TurnIt extends App {
  def transpose(A: List[List[Int]]): List[List[Int]] = A.filter(_.nonEmpty) match {
    case Nil => Nil
    case ys: List[List[Int]] => ys.map{_.head}::transpose(ys.map{_.tail} )
  }
}
