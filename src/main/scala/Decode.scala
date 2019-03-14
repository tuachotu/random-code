import scala.language.postfixOps
import scala.collection.mutable.HashMap

object Decode extends App {
  def validTwoDigit(number: Int) : Boolean = (9 < number) && (28 > number)
  def countMessage(message: String, lookup: HashMap[String, Int]): Int = {
    if (message.isEmpty) 1
    else if (message.head == '0') 0
    else if (lookup contains message) lookup(message)
    else {
      val count = countMessage(message.drop(1), lookup) +
                  ( if (validTwoDigit(message.take(2).toInt)) countMessage(message.drop(2), lookup) else 0 )
      lookup += (message -> count)
      count
    }
  }
  val message = args(0)
  println(message)
  println(countMessage(message, HashMap[String, Int]()))
}
