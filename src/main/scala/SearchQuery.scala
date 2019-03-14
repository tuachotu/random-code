import scala.language.postfixOps
import scala.collection.immutable.SortedSet

object SearchQuery {
  def longerRange(s1: Set[(Int,Int)], s2: Set[(Int,Int)]) : Set[(Int,Int)] = {
    if (s1.size >= s2.size) s1 else s2
  }
  // pseudo code
  // message = aaa bbb ccc ddd, query = aaa ccc ddd , size = 7
  // find all the matches in message , i,e,
  def trimMessage(message: String, query: String, size: Int): String = {
    val wordsInMessage = message.toLowerCase.split(" ")
    val wordsInQuery = query.toLowerCase.split(" +")
    val messageWordsWithIndexes =
      wordsInMessage zip wordsInMessage.scanLeft(0)((prev,next) => prev + next.length + 1) filter (_._1 != "")

    val matchesWithIndexes = wordsInQuery flatMap (word => messageWordsWithIndexes filter ( _._1 == word))
    val matchStartEnd = SortedSet[(Int,Int)]() ++ matchesWithIndexes.map(m => (m._2, m._2 + m._1.length)).toSet
    val goodMatches = matchStartEnd.subsets filter(x => !x.isEmpty && ( x.last._2 - x.head._1 <= size)) toSet
    val finalMatch = goodMatches reduceLeft longerRange

    message.substring(finalMatch.head._1, finalMatch.last._2)
  }

  def main(args: Array[String]):Unit =  {
    val message = args(0)
    val query = args(1)
    val size = args(2).toInt
    println(trimMessage(message, query, size))
  }
}
