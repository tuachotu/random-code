import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._


object FindPairSum extends App {
  def findPair(numbers: List[Int], targetSum: Int) : Array[Int] = {
    val lookup = HashMap[Int, Int]() // complement -> index
    var result = ArrayBuffer[Int]()
    breakable {
      numbers.indices foreach { index =>
        if (lookup contains numbers(index)) {
          result += lookup(numbers(index))
          result += index
          break
        }
        else lookup += (targetSum - numbers(index)) -> index
      }
    }
    result.toArray
  }

  println(findPair(List(2,7,11,15), 9).mkString(","))
}
