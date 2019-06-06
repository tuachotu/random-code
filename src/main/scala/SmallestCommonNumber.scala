object SmallestCommonNumber extends App {

  def findSmallest(lists : List[List[Int]]) : Int = lists.reduceLeft(_.intersect(_)).min

  println(findSmallest(List(List( 1, 3, 5, 10, 20 ), List( 2, 4, 5, 10, 20), List(2, 4, 10, 20)))) // gives 10
  println(findSmallest(List(List( 1, 3, 5, 10, 20 ), List( 2, 4, 5, 10, 20), List(2, 4, 10, 20), List(20)))) // gives 20
}
