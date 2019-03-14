object IdenticalBST extends App {

  def identical(l1: List[Int], l2:List[Int]): Boolean = {
    if (l1.length != l2.length) false
    else {
      (l1,l2) match {
        case (Nil,Nil) => true
        case (h1::Nil, h2::Nil) =>  h1 == h2
        case (h1::t1, h2::t2) =>
         /* h1 == h2 &&*/ (t1.min == t2.min) && (t1.max == t2.max) && identical(t1,t2)
      }
    }
  }

  val t1 =List(2, 4, 3, 1)
  val t2 = List(2, 1, 4, 3)
  println(identical(t1,t2))



  /*val t3 =List(8, 3, 6, 1, 4, 7, 10, 14, 13) ++ List(Int.MaxValue) ++ List(Int.MinValue)
  val t4 = List(8, 10, 14, 3, 6, 4, 1, 7, 13) ++ List(Int.MaxValue) ++ List(Int.MinValue)
  println(identical(t3,t4))*/


  val t3 =List(8, 3, 6, 1, 4, 7, 10, 114, 13)
  val t4 = List(8, 10, 14, 3, 6, 4, 1,17, 13)
  println(identical(t3,t4))

}
