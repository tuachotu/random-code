import scala.util.Random

object MergeSortedArray extends App {
  val list1 = List.fill(10)(Random.nextInt(100)).sorted
  val list2 = List.fill(10)(Random.nextInt(100)).sorted
  val list3 = List.fill(10)(Random.nextInt(100)).sorted
  val list4 = List.fill(5)(Random.nextInt(100)).sorted

  def merge(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (a, Nil) => a
      case (Nil, b) => b
      case (a::tailA, b::tailB) => if (a<b) a::merge(tailA, l2)
                                  else b::merge(l1, tailB)
    }
  }

  def mergeKSortedArray(l: List[List[Int]]): List[Int] = {
    l.filter(_.nonEmpty) match {
      case Nil => Nil
      case ll =>
        val a = ll.map(_.head).sorted
        val b = ll.map(_.tail)
        a.head::mergeKSortedArray(a.tail::b)
    }
  }

  println(list1.mkString(","))
  println(list2.mkString(","))
  //println(merge(list1,list2).mkString(","))
  println(list3.mkString(","))
  println(list4.mkString(","))
  println(mergeKSortedArray(List(list1,list2,list3,list4)).mkString(","))

}
