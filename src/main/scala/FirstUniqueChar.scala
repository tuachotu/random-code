
object FirstUniqueChar extends App {
  def firstUnique(s: String): Option[Char] = {
   val countMap =  (s groupBy (c=>c)) mapValues(_.length)
    def checkOccurence(s1: String ): Option[Char] = {
      if (countMap(s1.head) > 1) Some(s1.head)
      else if (s1.length == 1) None
      else checkOccurence(s1.tail)
    }
    checkOccurence(s)
  }
  println(firstUnique("abcdebC"))
  println(firstUnique("abcdef"))
}
