import scala.collection.mutable


object BSTCount extends App {
  // count(6) = count(5)*count(0) + count(4)*count(1) + count(3)*count(2)
  //            count(2).count(3) + count(1)*count(4) + count(0)*count(5)
  val lookup= mutable.HashMap[Int,Int]((0,1),(1,1),(2,2))
  def BSTCount(n: Int): Int = {
    n match {
      case 0 => 1
      case 1 => 1
      case 2 => 2
      case _  =>
        if (!(lookup contains n)) {
          val r = ((0 until n) map { i =>
            lookup += i -> BSTCount(i)
            lookup += (n-i-1) -> BSTCount(n-i-1)
            lookup(i) * lookup(n-i-1)
          }).sum
          lookup += (n -> r)
        }
        lookup(n)
    }
  }

  (1 to 10) foreach ( num => println(num + " -> " + BSTCount(num)))

}
