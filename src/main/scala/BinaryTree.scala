

abstract class BinaryTree {
  def value: Int
  def left: BinaryTree
  def right: BinaryTree
  def isEmpty: Boolean

  def add(v: Int): BinaryTree = {
    if (isEmpty) BinaryTreeNode(v)
    else if (value > v) BinaryTreeNode(value, left.add(v), right)
    else if (value < v) BinaryTreeNode(value, left, right.add(v))
    else this
  }


  def remove(v: Int): BinaryTree = {
    if (isEmpty) throw new NoSuchElementException("Node not found!!!")
    else if (value > v) BinaryTreeNode(value,left.remove(v), right)
    else if (value < v) BinaryTreeNode(value, left, right.remove(v))
    else  {
      if (left.isEmpty && left.isEmpty) EmptyTree
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else { // valid tree
        val replacement = right.min
        BinaryTreeNode(replacement, left,right.remove(replacement))
      }
    }
  }

  def min(): Int = {
    def findMin(t: BinaryTree, v: Int): Int = if (t.isEmpty) v else findMin(t.left, t.value)
    if (isEmpty) throw new NoSuchElementException("Empty Tree!!!")
    else  findMin(left, value)
  }

  def max(): Int = {
    def findMax(t: BinaryTree, v: Int): Int = if (t.isEmpty) v else findMax(t.right, t.value)
    if (isEmpty) throw new NoSuchElementException("Empty Tree!!!")
    else  findMax(right, value)
  }



  def preOrder(): Unit =
    if (!isEmpty) {
      print(value+",")
      left.preOrder
      right.preOrder
    }

  def postOrder(): Unit =
    if (!isEmpty) {
      left.postOrder
      right.postOrder
      print(value+",")
    }

  def inOrder(): Unit =
    if (!isEmpty) {
      left.inOrder
      print(value+",")
      right.inOrder
    }
}

case object EmptyTree extends BinaryTree {
  def value: Int =  throw new NoSuchElementException("Empty Tree!!!")
  def left: BinaryTree =  throw new NoSuchElementException("Empty Tree!!!")
  def right: BinaryTree =  throw new NoSuchElementException("Empty Tree!!!")
  def isEmpty: Boolean = true
}

case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyTree, right: BinaryTree = EmptyTree) extends BinaryTree {
  def isEmpty: Boolean = false
}

object MyBinaryTree extends App {

  /*val root = List(5, 15, 4, 122, 1, 6, 73, 4).foldLeft(EmptyTree.asInstanceOf[BinaryTree]) ( (t,v) => t.add(v))
  root.preOrder()
  println
  root.postOrder()
  println
  root.inOrder()
  println
  println(root.min)
  println
  println(root.max)

  (root.remove(5)).inOrder()*/
  def getAllBinaryTree(n: Int): List[BinaryTree] = {
    def getAllBinaryTreeInternal(l: List[Int]) : List[BinaryTree] = {
      l match {
        case Nil => List[BinaryTree](EmptyTree)
        case _ => l flatMap { root =>
          val leftSubTrees = getAllBinaryTreeInternal(l.filter(number => number < root ))
          val rightSubTrees = getAllBinaryTreeInternal(l.filter(number => number > root ))
          for {
            left <- leftSubTrees
            right <- rightSubTrees
          } yield BinaryTreeNode(root, left, right)
        }
      }
    }
    getAllBinaryTreeInternal((1 to n).toList)
  }

 /* getAllBinaryTree(4) foreach { bst =>
    println(bst)
  }*/

  def identical(bst1: BinaryTree, bst2: BinaryTree): Boolean = {
    if (bst1.isEmpty && bst2.isEmpty) true
    else if ((bst1.isEmpty && !bst2.isEmpty) || (!bst1.isEmpty && bst2.isEmpty)) false
    else bst1.value == bst2.value && identical(bst1.left, bst2.left) && identical(bst1.right, bst2.right)
  }

/*  val t1 = Seq(1,2,3,4,5,6).foldLeft(EmptyTree.asInstanceOf[BinaryTree]) ((t,v) => t.add(v))
  val t2 = Seq(1,2,3,4,5,6).foldLeft(EmptyTree.asInstanceOf[BinaryTree]) ((t,v) => t.add(v))

  println(identical(t1,t2))
*/
 /* val t1 = Seq(8, 3, 6, 1, 4, 7, 10, 14, 13).foldLeft(EmptyTree.asInstanceOf[BinaryTree]) ((t,v) => t.add(v))
  val t2 = Seq(8, 10, 14, 3, 6, 4, 1, 7, 13).foldLeft(EmptyTree.asInstanceOf[BinaryTree]) ((t,v) => t.add(v))



  println(identical(t1,t2))
  */





}