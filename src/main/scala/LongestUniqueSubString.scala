import scala.collection.mutable.HashMap

object LongestUniqueSubString extends App {
  def lengthOfLongestSubstring(s: String): Int = {
    if (s.isEmpty) 0
    else {
      val lookupTable = HashMap[Char,Int]()
      var start = 0
      var length = 0
      for ( (c, i) <- s.zipWithIndex) {
        if (!(lookupTable contains c)) {
          lookupTable(c) = i
        } else {
          val newLength = i - start
          if (newLength > length) length = newLength
          start = lookupTable(c) + 1
          lookupTable.retain((k,v) => v >= start)
          lookupTable(c) = i
        }
      }
      Math.max(length, (s.length - start))
    }
  }
  Seq("abcabcbb", "pwwkew", "", "nnnn", "b", "  ", "aab" , "abca" ,"abba") foreach { s =>
    println(s + " = " + lengthOfLongestSubstring(s))
  }
}

