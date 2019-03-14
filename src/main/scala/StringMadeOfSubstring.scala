import collection.mutable.ArrayBuffer

object StringMadeOfSubstring extends App {

  def lookupTable(s: String): ArrayBuffer[Int] = {
    // create an array buffer of same size
    val lookup = ArrayBuffer.fill(s.length)(-1)
    var i=0
    var len=0
    lookup(i) = len
    i+=1
    while (i<s.length) {
      if (s(i) == s(len)) {
        len += 1
        lookup(i) = len
        i += 1
      } else {
        if (len == 0) {
          lookup(i) = 0
          i += 1
        } else {
          len = lookup(len-1)
        }
      }
    }
    lookup
  }

  val  s = "ABABAB"
  val prefixes = lookupTable(s)
  println(prefixes.mkString(","))
  val possible = prefixes.indices map {index =>
    if (prefixes(index) != 0
        && (s.length - prefixes(index)) != 0
        && (s.length - prefixes(index) != 1)
        && (s.length % (s.length - prefixes(index)) == 0)) s.substring(0,s.length-index)
    else ""
  }

  println(possible.filter(!_.isEmpty).max)
}
