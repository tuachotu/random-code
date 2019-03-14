
import scala.collection.mutable.ArrayBuffer

object kmp extends App {
  def buildLookupTable(s: String): ArrayBuffer[Int] = {
    val lookupTable = ArrayBuffer.fill(s.length)(-1)
    lookupTable(0) = 0 // first char always 0
    var len = 0
    var i = 1
    while( i < s.length) {
      if (s(i) == s(len)) {
        len += 1
        lookupTable(i) = len
        i += 1
      } else { // mismatch
        if (len == 0) {
          lookupTable(i) = 0;
          i= i+1
        } else {
          len = lookupTable(len-1)
        }
      }
    }
    lookupTable
  }

  def searchPattern(s: String, p: String): Unit = {
    if (p.length > s.length) println("bad input")
    else if (p == s) println("same strings")
    else {
      val lookupTable = buildLookupTable(p)

      var i= 0 // for s
      var j= 0 // for p

      while (i < s.length) {
        if (s(i) == p(j)) {
          i += 1
          j += 1
        }
        if (j == p.length) {
          println(s"pattern found at ${i-j}")
          j = lookupTable(j-1)
        }
        else {
          if (i < s.length && s(i) != p(j)) {
            if (j != 0) j = lookupTable(j-1)
            else i+=1
          }
        }
      }
    }
  }


  println("AAAA -> " +  buildLookupTable("AAAA").mkString(","))
  println("ABCDE  -> " + buildLookupTable("ABCDE").mkString(","))
  println("AABAACAABAA -> "+buildLookupTable("AABAACAABAA").mkString(","))

  println(searchPattern("aababaaa", "aba"))



}
