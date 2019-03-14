import scala.collection.mutable

object WordLadder extends App {
  val dictionary = Set("hot", "dot", "dog", "lot", "log", "cog")
  def oneStepAway(w1: String, w2: String): Boolean = (w1 diff w2).length == 1
  def calculateSteps(start: String, end: String): Int = {
    val lookupTable = mutable.Queue[(String, Int, Array[String])]((start, 0, Array[String]()))
    var matchNotFound = true
    var stepsTaken = 0
    while (matchNotFound && lookupTable.nonEmpty) {
      val (word, distance, path) = lookupTable.dequeue()
      if (word == end) {
        matchNotFound = false
        stepsTaken = distance
        println(path ++ Array(end) mkString(" -> "))
      } else {
        dictionary foreach { entry =>
          if (oneStepAway(word, entry) && !lookupTable.find(_._1 == entry).isDefined)  {
            lookupTable.enqueue((entry, distance+1, path ++ Array(word)))
          }
        }
      }
    }
    if (!matchNotFound) stepsTaken else -1
  }
  println(calculateSteps("hit", "cog"))
}
