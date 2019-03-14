object IsEqualBinaryTree extends App {

  def identicalBSTs(t1: List[Int], t2: List[Int]): Boolean = {
    (t1,t2) match {
      case (Nil, Nil) => true
      case (Nil, y) => false
      case (x, Nil) => false
      case _ => // valid lists
        if (t1.head != t2.head) false
        else {
          identicalBSTs(t1.filter( n => n  < t1.head),t2.filter(n => n < t2.head)) &&
            identicalBSTs(t1.filter( n => n  > t2.head),t2.filter(n => n > t2.head) )
        }
    }
  }
  println(identicalBSTs(List(8, 3, 6, 1, 4, 7, 10, 14, 13),List(8, 3, 6, 1, 4, 7, 10, 14, 13)))
}
