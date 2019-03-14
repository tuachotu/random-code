
import scala.language.postfixOps
import scala.collection.immutable.SortedSet

object FindMaximumMatches extends App {

  def trimMessage(message: String, query: String, size: Int): String = {
    def longerRange(s1: Set[(Int,Int)], s2: Set[(Int,Int)]) : Set[(Int,Int)] = {
      if (s1.size >= s2.size) s1 else s2
    }
    if (size > message.length) message else {
      val wordsInMessage = message.toLowerCase.split(" ")
      val wordsInQuery = query.toLowerCase.split(" +")
      val messageWordsWithIndexes =
        wordsInMessage zip wordsInMessage.scanLeft(0)((prev,next) => prev + next.length + 1) filter (_._1 != "")

      val matchesWithIndexes = wordsInQuery flatMap (word => messageWordsWithIndexes filter ( _._1 == word))
      val matchStartEnd = SortedSet[(Int,Int)]() ++ matchesWithIndexes.map(m => (m._2, m._2 + m._1.length)).toSet
      val goodMatches = matchStartEnd.subsets filter(x => !x.isEmpty && ( x.last._2 - x.head._1 <= size)) toSet

      if (goodMatches.isEmpty) { "error" } else {
        val finalMatch = goodMatches reduceLeft longerRange
        val (matchStart, matchEnd) = (finalMatch.head._1, finalMatch.last._2)
        if (matchEnd - matchStart < size) {
          val requiredChars = size - (matchEnd - matchStart)
          val (start, end) = if (matchEnd + requiredChars < message.length) (matchStart, matchEnd + requiredChars)
                             else (matchStart - requiredChars, matchEnd)
          message.substring(start, end)
        } else message.substring(finalMatch.head._1, finalMatch.last._2)
      }
    }
  }

  val message = args(0)
  val query = args(1)
  val size = args(2).toInt
  println(trimMessage(message, query, size))
}
