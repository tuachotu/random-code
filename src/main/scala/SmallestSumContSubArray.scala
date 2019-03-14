object SmallestSumContSubArray extends App {
  val numbers = List(3, -4, 2, -3, -1, 7, -5)
  println(numbers.mkString(","))
  def minOfThree(x:Int, y: Int, z: Int) : Int = Math.min(x, Math.min(y,z))

  def smallestSum(l: List[Int]): Int = {
    l match {
      case Nil => Int.MaxValue
      case x:: Nil => x
      case _ =>
        val mid = l.length / 2
        val (left, right) = l splitAt mid
        minOfThree(smallestSum(left),smallestSum(right), l.sum)
    }
  }

  println(smallestSum(numbers))

}
