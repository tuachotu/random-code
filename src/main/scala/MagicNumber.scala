import scala.collection.mutable.Set

object HappyNumber extends App {

  def time[A](f:A): A ={
    val t = System.nanoTime()
    val r = f
    println("time = " + (System.nanoTime() - t) + "  ns")
    r
  }

  def findSquareSum(n: Int): Int =
    n.toString.foldLeft(0) { (product, num) => product + num.asDigit * num.asDigit }

  val visited = Set[Int]()



  def isHappyNumber(n: Int): Boolean = {
    n match {
      case 1 => true
      case _  =>
        if (visited contains n) false
        else {
          visited += n
          if (isHappyNumber(findSquareSum(n))) { visited -= n; true} else false
        }
    }
  }
  def memo[A,B,R](f :(A,B)=>R): (A,B)=>R = {
    val cache = new collection.mutable.WeakHashMap[A,R]
    (a:A,b:B) => cache.getOrElseUpdate(a,f(a,b))
  }

  //isHappyNumer() is now a memoized function
  // for quick lookup of both happy and unhappy numbers
  //
  val isHappyNumber1 :(Int, Set[Int]) => Boolean = memo { (n, seen) =>
    if (n == 1) true
    else if (seen(n)) false
    else isHappyNumber1(findSquareSum(n), seen + n)
  }


  time { (1 to 247).filter(isHappyNumber(_)).foreach(println) }

  time { (1 to 247).filter(isHappyNumber1(_, Set())).foreach(println) }
}
