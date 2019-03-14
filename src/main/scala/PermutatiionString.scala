import scala.collection.mutable.ArrayBuffer
object StringPermutaions extends App {
  def permutations(prefix: String, suffix: String, result: ArrayBuffer[String]) : Unit = {
    if (suffix.isEmpty) result += prefix
    else {
      permutations(prefix + suffix.take(1), suffix.drop(1), result)
    }
  }
  val result = ArrayBuffer[String]()
  permutations("", args(0), result)
  result foreach println
}
