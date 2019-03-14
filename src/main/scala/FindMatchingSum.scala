import scala.collection.immutable._

object FindMatchingSum {
  def containsMatchingSum(data: Array[Int], complementTable: Set[Int], sum: Int): Boolean = {
    if (data.isEmpty) false
    else if (complementTable contains data.head) true
    else containsMatchingSum(data.drop(1), complementTable ++ Set(sum-data.head), sum)
  }

  def main(args: Array[String]):Unit = {
    println(containsMatchingSum(Array(1,2,4,5,6), Set[Int](), 7))
    println(containsMatchingSum(Array(1,1,1,5,6), Set[Int](), 2))
    println(containsMatchingSum(Array(1,4,4,5,6), Set[Int](), 9))
    println(containsMatchingSum(Array(1,4,4,5,6), Set[Int](), 13))
    println(containsMatchingSum(Array(1,4,4,5,6), Set[Int](), 11))
  }
}
