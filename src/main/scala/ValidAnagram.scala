import scala.collection.immutable.HashMap

object ValidAnagram extends App {
def time[T](f:T): T = {
  val t = System.nanoTime
  val r = f
  println("time = " + (System.nanoTime -t ) + " ns")
  r
}

implicit class Counter(s: String) {
    // this will be "added" to the String API via an implicit class construction
    def countCharacters: Map[Char, Int] =
      s.foldLeft(Map.empty[Char, Int])({ case (acc, c) =>
        acc + (c -> (acc.getOrElse(c, 0) + 1))
      })
  }

  def validAnagram1(s1: String, s2: String): Boolean =
    if (s1.length != s2.length) false
    else if (s1 == s2) true
    else s1.countCharacters == s2.countCharacters

  def validAnagram(s1: String, s2: String): Boolean = {
    if (s1 == s2) true
    else if (s1.length != s2.length) false
    else {
      val lookupTable = s1.foldLeft(HashMap[Char,Int]()) ((m,c) => m ++ HashMap(c -> (if (m contains c) m(c) + 1 else 1)))
      (s2.foldLeft(lookupTable) {(m,c) =>
        if (m contains c) {
          val count = m(c)
          if (count > 1) {m + (c -> (count-1))} else m - c
        } else m  // we can return here if not functional
      }).isEmpty
    }
  }
    println(time {validAnagram(args(0), args(1))})
    println(time {validAnagram1(args(0), args(1))})
    println(time {args(0).sorted == args(1).sorted })
  }
