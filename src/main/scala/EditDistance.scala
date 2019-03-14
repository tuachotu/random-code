import scala.collection.mutable

object EditDistance extends App {

  def time[A](f:A): A = {
    val start = System.nanoTime()
    val r = f
    println("time taken is " + (System.nanoTime - start) )
    r
  }

  def minOfThree(a: Int, b: Int, c: Int): Int = Math.min(a, Math.min(b,c))

  def editDistance(s1: String, s2: String): Int = {
    if (s1.length == 0) s2.length
    else if (s2.length == 0) s1.length
    else if(s1.head == s2.head)  editDistance(s1.tail, s2.tail)
    else {
      1 + minOfThree(editDistance(s1, s2.tail),
                     editDistance(s1.tail, s2),
                     editDistance(s1.tail, s2.tail))
    }
  }

  val lookupTable = mutable.HashMap[String,Int]()

  def editDistanceR(s1: String, s2: String): Int = {
    if (lookupTable contains s1+s2) lookupTable(s1+s2)
    else {
      val distance = if (s1.length == 0) s2.length
        else if (s2.length == 0) s1.length
        else if (s1.head == s2.head) editDistance(s1.tail, s2.tail)
        else {
          1 + minOfThree(editDistance(s1, s2.tail),
                         editDistance(s1.tail, s2),
                         editDistance(s1.tail, s2.tail))
        }
      lookupTable += (s1+s2 -> distance)
      distance
    }
  }

  println(time {editDistance("sunday", "monday")})


  println(time {editDistance("sunday", "monday")})

}
