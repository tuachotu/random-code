import scala.util.Random

object Sorting extends App {

  def mergeSort(l: List[Int]): List[Int] = {
    val mid = l.length / 2
    if (mid == 0 ) l
    else {
      def merge(left: List[Int], right: List[Int]): List[Int] = {
        (left, right) match {
          case (Nil, right) => right
          case (left, Nil) => left
          case (lx::ll, rx::rr) =>
            if (lx < rx)  lx::merge(ll , right)
            else  rx::merge(left, rr)
        }
      }
      val (left, right) = l.splitAt(mid)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def quickSort(l: List[Int]): List[Int] = {
    val mid = l.length / 2
    if (mid == 0) l
    else {
      val pivot = l(mid)
      quickSort(l.filter(_ < pivot)) ++  l.filter(pivot ==) ++ quickSort(l.filter(_ > pivot))
    }
  }

  val numbers = List.fill(10)(Random.nextInt(9))
  println(numbers.mkString(","))
  println(mergeSort(numbers) mkString(","))
  println(quickSort(numbers) mkString(","))


}
