object MaxSubArray extends App {
  def maxOfThree(a:Int, b:Int, c:Int): Int = Math.max(a, Math.max(b,c))

  def maxSubArray(l: List[Int]) : Int = {
    l match {
      case List() => 0
      case List(x) => x
      case _ =>
        val middle = l.length / 2
        val (left, right) = l.splitAt(middle)
        val sumOfArray = left.reduceLeft(_+_) + right.reduceLeft(_+_) - l(middle)
        maxOfThree(maxSubArray(left), maxSubArray(right), sumOfArray)
    }
  }

  println(maxSubArray(List(-2, -5, 6, -2, -3, 1, 5, -6)))
}
