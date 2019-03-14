import scala.collection.immutable.HashMap

object CheckValidSentence extends App {
  def time[A](f:A):A = {
    val start = System.nanoTime
    val r = f
    print("time taken = " + (System.nanoTime - start) + " ns  ;")
    r
  }
  val dictionary: Set[String] = Set("this","is", "a", "word","there","may","be","cat","the")
  //val dictionary: Set[String] = Set("apple", "pen")



  //val dictionary: Set[String] = Set("a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa")


  def validSentence(s: String, prefix: String = ""): Boolean = if (s.isEmpty) false else {
    (if (dictionary contains (prefix + s.head)) { if (s.length == 1) true else validSentence(s.tail) } else false ) ||
      validSentence(s.tail, prefix + s.head)
    }


  /*val lookup = scala.collection.mutable.HashMap[String, Boolean]()

  def validSentenceFaster(s: String, prefix: String = ""): Boolean = if (s.isEmpty) false else {
    (if (lookup.getOrElse(prefix+s.head, false))  { if (s.length == 1) true else validSentence(s.tail) }
     else if (dictionary contains (prefix + s.head)) {
      lookup += (prefix + s.head) -> true
      if (s.length == 1) true
      else validSentenceFaster(s.tail)
    } else false ) || validSentenceFaster(s.tail, prefix + s.head)
  }

*/
  val lookup = scala.collection.mutable.HashMap[(Int, Int), Boolean]()

  def validSentenceItrFaster(s: String, startIndex: Int = 0, endIndex: Int = 0): Boolean =
    if (s.isEmpty) false else {
        (if (lookup.getOrElse((startIndex, endIndex), false)) {
          if (s.length == endIndex + 1) true
          else validSentenceItrFaster(s, endIndex+1, endIndex + 1)
        } else if  (endIndex >= s.length) false
         else if (dictionary contains s.substring(startIndex, endIndex + 1))  {
          lookup += (startIndex, endIndex) -> true
          if (s.length == endIndex + 1) true
          else validSentenceItrFaster(s, endIndex+1, endIndex + 1)
        } else false) || validSentenceItrFaster(s, startIndex, endIndex+1)
    }



  def validSentenceRev(s: String, suffix: String = ""): Boolean = if (s.isEmpty) false else {
    (if (dictionary contains (s.last + suffix)) { if (s.length == 1) true else validSentenceRev(s.init) } else false ) ||
      validSentenceRev(s.init,  s.last + suffix)
  }


  println(validSentenceItrFaster("applepenapple"))
  lookup.clear()
  println(validSentenceItrFaster("theremaybe"))
  lookup.clear()
  println(validSentenceItrFaster("thismaybe"))
  lookup.clear()
  println(validSentenceItrFaster("theremaynotbe"))
  lookup.clear()
  println(validSentenceItrFaster("theremaynotbesdasfasf"))
  //lookup.clear()
  //println(time(validSentenceFaster("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab")))
  lookup.clear()
  //println(time(validSentenceFaster("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
  //println(time(validSentenceRev("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
  /*println("aaaab = " +  time(validSentence("aaaab")))
  println("aaaaab = " +  time(validSentence("aaaaab")))
  println("aaaaaab = " +  time(validSentence("aaaaaab")))
  println("aaaaaaab = " +  time(validSentence("aaaaaaab")))
  println("aaaaaaaab = " +  time(validSentence("aaaaaaaab")))
  println("aaaaaaaaab = " +  time(validSentence("aaaaaaaaab")))
  println("aaaaaaaaaab = " +  time(validSentence("aaaaaaaaaab")))
  println("aaaaaaaaaaab = " +  time(validSentence("aaaaaaaaaaab")))
  println("aaaaaaaaaaaab = " +  time(validSentence("aaaaaaaaaaaab")))
  println("aaaaaaaaaaaaab = " +  time(validSentence("aaaaaaaaaaaaab")))
  println("aaaaaaaaaaaaaab = " +  time(validSentence("aaaaaaaaaaaaaaab")))
  println("aaaaaaaaaaaaaaab = " +  time(validSentence("aaaaaaaaaaaaaaaa")))
*/
 }
