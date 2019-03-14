object SmallestPostiveNumber extends App{

  def solution(a: Array[Int]): Int = {
    val allPositives = a filter (_ > 0)
    val replaceArray = Array.fill(allPositives.length)(0)


    allPositives.zipWithIndex foreach {number => replaceArray(number._2) = - number._1}
    println(replaceArray.mkString(","))

    replaceArray.zipWithIndex.collectFirst {case (x,y) if x > 0 => x} match {
      case Some(x) => x
      case _ => -1
    }
  }

  val input = Array(1, 3, 6, 4, 1, 2)
  println(solution(input))

}
