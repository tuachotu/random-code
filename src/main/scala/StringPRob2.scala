/*
Given a string S.
Count number of substrings in which each character occurs at most k times.
Assume that the string consists of only lowercase English alphabets.

Examples:

Input : S = ab
        k = 1
Output : 3

All the substrings a, b, ab have
individual character count less than 1.

Input : S = aaabb
        k = 2
Output : 12
Substrings that have individual character
count at most 2 are: a, a, a, b, b, aa, aa,
ab, bb, aab, abb, aabb.
 */

object StringPRob2 extends App {
  def combinations(l: List[Char], n: Int): List[List[Char]] =
    if (n == 1) {
      l.map(List(_))
    } else {
      l match {
        case h :: t =>
          combinations(t, n - 1).map(h :: _) ++ combinations(t, n)
        case _ => List()
      }
    }

  def getCombinations(chars: List[Char]): List[List[Char]] = {
    (for {
      i <- 1 to chars.length
      c = combinations(chars, i)
    } yield c).toList.flatten
  }

  def getMatch(s: String, k: Int): Set[String] ={
    (getCombinations(s.toList) map (_.mkString(""))).filter { c =>
       val l = c.groupBy(x=> x).toList map (c => (c._1, c._2.length)) filter (pair => pair._2 > k)
       l.isEmpty
     } toSet

  }
  getMatch("aaabb",1) foreach println
 // getCombinations("aaabb".toList).filter(_.nonEmpty) foreach println


}
