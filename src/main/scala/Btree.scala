

import scala.collection.mutable._

abstract class MyTree {
    def value: Int
    def left: MyTree
    def right: MyTree
    def isEmpty: Boolean
  }
  case object NullTree extends MyTree {
    def value: Int =  throw new NoSuchElementException("Empty Tree!!!")
    def left: MyTree =  throw new NoSuchElementException("Empty Tree!!!")
    def right: MyTree =  throw new NoSuchElementException("Empty Tree!!!")
    def isEmpty: Boolean = true
  }
  case class MyBinaryTreeNode(value: Int, left: MyTree = NullTree, right: MyTree = NullTree) extends MyTree {
    def isEmpty: Boolean = false
  }

  object Btree extends App {
    // create Binary tree from the problem
    val root = MyBinaryTreeNode(1, // root
                                MyBinaryTreeNode(2,NullTree,MyBinaryTreeNode(4)), // left
                                MyBinaryTreeNode(3,
                                                 MyBinaryTreeNode(5, MyBinaryTreeNode(7), MyBinaryTreeNode(8)),
                                                 MyBinaryTreeNode(6, MyBinaryTreeNode(9), MyBinaryTreeNode(10,
                                                                                          MyBinaryTreeNode(11)))
                                                )
                                ) // right

    def countEmptyOrFullNodes(t: MyTree): Int = {
     if (t.isEmpty) 0
     else if (t.left.isEmpty && t.right.isEmpty) 1
     else if (t.left.isEmpty || t.right.isEmpty) 0
     else 1 + countEmptyOrFullNodes(t.left) + countEmptyOrFullNodes(t.right)
    }

    def CountPossiblePerfectTrees(t: MyTree): HashMap[Int, Int] = {
      val lookup = HashMap[Int, Int]()
      if (!t.isEmpty) {
        lookup(t.value) =  countEmptyOrFullNodes(t)
        CountPossiblePerfectTrees(t.left)
        CountPossiblePerfectTrees(t.right)
      }
       lookup
    }

    println(CountPossiblePerfectTrees(root).values.max)  // sol 1 I wanted to test
    println(countEmptyOrFullNodes(root))  // sol 2 I wanted to test
  }
