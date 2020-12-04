package inclass8

sealed trait Tree
case object Empty extends Tree
case class Node(left: Tree, key: Int, right: Tree) extends Tree
object inclass8 extends App {

  def fibCPS(n: Int): Int = {
    def helperCPS(n: Int, K: Int => Int): Int = {
      n match {
        case 0 => K(0)
        case 1 => K(1)
        case _ =>
          helperCPS(n - 1, (a: Int) => helperCPS(n - 2, (b: Int) => K(a + b)))
      }
    }

    helperCPS(n, (x: Int) => x)
  }

  def walkPreorder(t: Tree): List[Int] = {
    def contWalk(t: Tree, K: List[Int] => List[Int]): List[Int] = t
    match {
      case Empty => K(Nil)
      case Node(l, k, r) => contWalk(l, leftList => {
        contWalk(r, rightList => K(k :: (leftList ::: rightList)))
      })
    }

    contWalk(t, (r: List[Int]) => r)
  }
  val t = Node(Node(Empty, 2, Empty),
    5,
    Node(Node(Empty, 6, Empty), 7, Node(Empty, 9, Empty)))

  println(fibCPS(4))
  println(walkPreorder(t))
}

