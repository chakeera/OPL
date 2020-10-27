import scala.math

/* Since this is a open book exam, I would like to give credits to
stack overflow, github, scala.user help manual, youtube, Ajarn rachata's videos and lecture (watched a few times)
oh oh and Niclas Hedam github :) */
class bst[A](implicit ord: Ordering[A]) {
  /* Declaring the tree with generic type A which means any type it may be.
  Declaring BST components as a class like the branch that includes left node, root/key and right node.
  the Leaf object here is when it is doesnt have a child.
   */
   sealed trait Tree[A]
   case object Leaf extends Tree[A]
   case class Branch[A](left: Tree[A], k:A , right: Tree[A]) extends Tree[A]

  /*Thanks to stackoverflow for the logic*/
  def insertCPS(t:Tree[A], newkey:A): Tree[A]={
    def helperfunc(t:Tree[A], temp:Tree[A]=>Tree[A],newkey:A): Tree[A] = t match{
      case Leaf => temp(Branch(Leaf, newkey, Leaf))
      case Branch(leftChild, key, rightChild) if (key == newkey) => temp(Branch(leftChild, key, rightChild))
      case Branch(leftChild, key, rightChild) if (ord.lt(newkey,key)) => helperfunc(leftChild, func=>temp(Branch(func,key, rightChild)),newkey)
      case Branch(leftChild, key, rightChild) => helperfunc(rightChild, func=>temp(Branch(leftChild, key, func)),newkey)
    }
    helperfunc( t, (r:Tree[A]) =>r,newkey)
  }

  /*This is the walkCPS that ajarn have shown in the class using inorder transversal */
    def walkCPS[A](t: Tree[A]): List[A] = {
      def contWalk(t: Tree[A], lst: List[A] => List[A]): List[A] = t match {
        case Leaf => lst(Nil)
        case Branch(left, key, right) => contWalk(left, leftList => {
          contWalk(right, rightList => lst(leftList ::: (key :: rightList)))
        })
      }
      contWalk(t, (p:List[A]) => p)
}
  /* Using Tail recursion, Here we will check the left and then right and get the max one plus 1 for the root*/
  def heightCPS[A](t: Tree[A]): Int = {
      def helperfunc(t: Tree[A], K: Int => Int): Int = t match {
        case Leaf => K(0)
        case Branch(left, key, right) => helperfunc(left, leftH => {
          helperfunc(right, rightH => K(1 + math.max(leftH, rightH)))
        })
      }
      helperfunc(t, (a: Int) => a)
    }

  /*"If a binary tree is traversed in-order, the output will produce sorted key values in an ascending order."- tutorialpoint definition*/
  def checkBST(t:Tree[A]):Boolean ={
    if(walkCPS(t)== walkCPS(t).sorted){
      true
    }
    else{
      false
    }

  }}
  object bstMain extends App {
    val tree1 = new bst[Int]()
    val t = tree1.Branch(tree1.Branch(tree1.Leaf, 8, tree1.Leaf), 1, tree1.Branch(tree1.Branch(tree1.Leaf, 3, tree1.Leaf), 4, tree1.Branch(tree1.Leaf, 9, tree1.Leaf)))
    println(tree1.walkCPS(t))
    println(tree1.heightCPS(t))//height
    val tt = tree1.insertCPS(t,20)
    println(tree1.walkCPS(tt))
    println(tree1.heightCPS(tt))//height
    println(tree1.checkBST(tt))
    println(tree1.checkBST(tt))

    val tree2 = new bst[Int]()
    val t2 = tree2.Branch(tree2.Branch(tree2.Branch(tree2.Leaf, 1, tree2.Leaf), 3,tree2.Branch(tree2.Branch(tree2.Leaf,4, tree2.Leaf),6,tree2.Branch(tree2.Leaf, 7, tree2.Leaf))),8,tree2.Branch(tree2.Leaf, 10, tree2.Branch(tree2.Branch(tree2.Leaf, 13, tree2.Leaf),14, tree2.Leaf)))
    println(tree2.walkCPS(t2))
    println(tree2.heightCPS(t2))//height
    val tt2 = tree2.insertCPS(t2,20)
    println(tree2.walkCPS(tt2))
    println(tree2.heightCPS(tt2))//height
    println(tree2.checkBST(tt2))
    println(tree2.checkBST(tt2))


}

