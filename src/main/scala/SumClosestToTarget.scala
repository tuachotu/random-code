object SumClosestToTarget extends App {

  val nums = List(-1,2,1,-4)

  def combinations(l: List[Int], c: Int): List[List[Int]] = {
    if (c == 1) { l map { r => List(r) } }else {
      l match {
        case Nil => List.empty
        case head::tail =>
          (combinations(tail, c-1) map { r => head :: r }) ++ combinations(tail, c)
      }
    }
  }

  combinations(nums, 3) foreach(combination => println(combination.mkString(",") + "=>" +
    (1 -  combination.reduceLeft(_+_))))


  //combinations(nums, 3) foldLeft(0) ()

}
