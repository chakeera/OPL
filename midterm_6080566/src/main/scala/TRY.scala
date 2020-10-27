import scala.math.max

object BST extends App {

  class BST[K](implicit ord: Ordering[K]) {

    sealed trait BSTree[K]

    case object Empty extends BSTree[K]

    case class Node[K](leftChild: BSTree[K], key: K, rightChild: BSTree[K]) extends BSTree[K]

    def insertCPS(k: K, t: BSTree[K]): BSTree[K]={
      def helper(k:K, t:BSTree[K], ans:BSTree[K]=>BSTree[K]): BSTree[K] = t match{
        case Empty => ans(Node(Empty, k, Empty))
        case Node(leftChild, key, rightChild) if (key == k) => ans(Node(leftChild, key, rightChild))
        case Node(leftChild, key, rightChild) if (ord.lt(k, key)) =>helper(k, leftChild, temp =>ans(Node(temp,key, rightChild)))
        case Node(leftChild, key, rightChild) => helper(k, rightChild, temp =>Node(leftChild, key, temp))
      }
      helper(k, t, (r:BSTree[K]) =>r)
    }

    def heightCPS(t: BSTree[K]): Int = {
      def helperHCPS(t: BSTree[K], K: Int => Int): Int = t match {
        case Empty => K(0)
        case Node(leftChild, key, rightChild) => helperHCPS(leftChild, leftHeight => {
          helperHCPS(rightChild, rightHeight => K(1 + max(leftHeight, rightHeight)))
        })
      }
      helperHCPS(t, (r: Int) => r)
    }


    def checkBST(t:BSTree[K]) : Boolean ={
      walkCPS(t) == walkCPS(t).sorted
    }


    def walkCPS[K](t: BSTree[K]): List[K] = {
      def contWalk(t: BSTree[K], lst: List[K] => List[K]): List[K] = t match {
        case Empty => lst(Nil)
        case Node(leftChild, key, rightChild) => contWalk(leftChild, leftList => {
          contWalk(rightChild, rightList => lst(leftList ::: (key :: rightList)))
        })
      }

      contWalk(t, (r: List[K]) => r)
    }

  }
  // Test driver
  val tree = new BST[Int]()
  val t = tree.Node(tree.Node(tree.Empty, 2, tree.Empty), 5, tree.Node(tree.Node(tree.Empty, 6, tree.Empty), 7, tree.Node(tree.Empty, 9, tree.Empty)))
  Console.println(tree.walkCPS(t))
  Console.print("Height: ")
  Console.println(tree.heightCPS(t))
  val t2 = tree.insertCPS(11, t)
  Console.println("Adding: 11")
  Console.println(tree.walkCPS(t2))
  Console.print("Height: ")
  Console.println(tree.heightCPS(t2))
  val t3 = tree.insertCPS(3, t2)
  Console.println("Adding: 3")
  Console.println(tree.walkCPS(t3))
  Console.print("Height: ")
  Console.println(tree.heightCPS(t3))
  val t4 = tree.insertCPS(29, t3)
  Console.println("Adding: 29")
  Console.println(tree.walkCPS(t4))
  Console.print("Height: ")
  Console.println(tree.heightCPS(t4))
  val t5 = tree.insertCPS(18, t4)
  Console.println("Adding: 18")
  Console.println(tree.walkCPS(t5))
  Console.print("Height: ")
  Console.println(tree.heightCPS(t5))
  // check BST
  Console.println(tree.checkBST(t))
  Console.println(tree.checkBST(t2))
  Console.println(tree.checkBST(t3))
  Console.println(tree.checkBST(t4))
  Console.println(tree.checkBST(t5))
  val notBST = tree.Node(tree.Node(tree.Empty, 2, tree.Empty), 5, tree.Node(tree.Node(tree.Empty, 88, tree.Empty), 7, tree.Node(tree.Empty, 9, tree.Empty)))
  Console.print(tree.walkCPS(notBST))
  //  Console.println(" In-order Traversal doesn't return sorted from low-high")
  Console.println(tree.checkBST(notBST))

  val treeD = new BST[Double]()
  val tD = treeD.Node(treeD.Node(treeD.Empty, 2.2, treeD.Empty), 5.9, treeD.Node(treeD.Node(treeD.Empty, 6.3, treeD.Empty), 7.1, treeD.Node(treeD.Empty, 9.9, treeD.Empty)))
  Console.println(treeD.walkCPS(tD))
  Console.print("Height: ")
  Console.println(treeD.heightCPS(tD))


}






