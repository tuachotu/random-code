import scala.collection.mutable.ArrayBuffer

object fibonacci {
  def fib_rec(n: Int): Int = {
    if (n == 1 || n == 2 ) 1
    else fib_rec(n-1) + fib_rec(n-2)
  }

  def fib_iter_slim(n: Int) : BigInt = {
    var last : BigInt = BigInt(1)
    var lastToLast : BigInt = BigInt(1)
    var result : BigInt= BigInt(0)
    for (i <- 3 to n ) {
      result = last + lastToLast
      lastToLast = last
      last = result
    }
    result
  }

  def fib_iter(n: Int, memo: ArrayBuffer[Option[BigInt]]) : BigInt = {
    for (i <- 1 to n ) {
      memo(i) = if (i == 1L || i == 2L) {
        Some(BigInt(1))
      } else {
        Some(memo(i-1).get + memo(i-2).get)
      }
    }
    memo(n).get
  }

  def fib_rec_memo(n: Int, memo: ArrayBuffer[Option[BigInt]]) : BigInt = {
    memo(n) match {
      case Some(value) => value
      case _ =>
        if (n == 1L || n == 2L) {
          memo(n) = Some(BigInt(1))
        } else {
          memo(n) = Some(fib_rec_memo(n-1, memo) +  fib_rec_memo(n-2, memo))
        }
        memo(n).get
    }
  }

  def main(args: Array[String]): Unit = {
    val num = args(0).toInt
    val memo: ArrayBuffer[Option[BigInt]] = ArrayBuffer.fill(num+1)(None)
    println("Hello world")
   /* val t0 = System.nanoTime()
    for (i <- 1 to 10) print(s"${fib_rec(i)} ")
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
   */ println
    /*val t2 = System.nanoTime()
     println(s" $num -> ${fib_rec_memo(num, memo)} ")
    val t3 = System.nanoTime()
    println("Elapsed time: " + (t3 - t2) + "ns")
*/
    println
    val memo1: ArrayBuffer[Option[BigInt]] = ArrayBuffer.fill(num+1)(None)
    val t4 = System.nanoTime()
     println(s" $num -> ${fib_iter(num, memo1)} ")
    val t5 = System.nanoTime()
    println("Elapsed time: " + (t5 - t4) + "ns")

    println
    val t6 = System.nanoTime()
     println(s" $num -> ${fib_iter_slim(num)} ")
    val t7 = System.nanoTime()
    println("Elapsed time: " + (t7 - t6) + "ns")



   /* val t4 = System.nanoTime()
    val fib1000 = fib_rec(100)
    val t5 = System.nanoTime()
    println(s"Fibonacci Number for 100 = $fib1000")
    println("Elapsed time: " + (t5 - t4) + "ns")
    val memo2: ArrayBuffer[Option[Long]] = ArrayBuffer.fill(101)(None)
    val t6 = System.nanoTime()
    val fib1000_2: Long = fib_rec_memo(100, memo2)
    println(s"Fibonacci Number for 100 = $fib1000_2")
    val t7 = System.nanoTime()
    println("Elapsed time: " + (t7 - t6) + "ns")
    */
  }
}
