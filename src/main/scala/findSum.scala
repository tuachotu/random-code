object FindSumCombination {
  def searchCombination(input: Array[Int], targetSum: Int, collection: Array[Int]) : Unit = {
    if (targetSum == 0)  {
      scala.util.Sorting.quickSort(collection)
      println(collection.mkString(","))
    } else {
      input foreach { number =>
        val newTargetSum = targetSum - number
        if (newTargetSum >= 0)
          searchCombination(input, newTargetSum, collection ++ Array(number))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Array(2,3,5)
    val targetSum = 12
    println("input =" + input.mkString(","))
    println(s"targetSum  = $targetSum" )

    input foreach { number =>
      searchCombination(input, targetSum - number, Array(number))
    }
  }
}


