import jdk.nashorn.internal.objects.ArrayBufferView

import collection.mutable.{ArrayBuffer,Queue}
import scala.annotation.tailrec

object ReverseInteger extends App {

   def reverse(x: Int): Int = {

     //val lookup = Queue[Int]()
     val lookup = ArrayBuffer[Int]()

     @tailrec
       def fillQ(n: Int) : Unit = {
         //lookup.enqueue(n%10)
       lookup+=(n%10)
         if ((n/10)>0) fillQ(n/10)
       }
     var y = math.abs(x)
  /*   while(y > 0) {
       lookup.enqueue(y%10)
       y = y / 10
     }
*/
     fillQ(y)
     try {
       val res = lookup.fold(0) { case (sum, number) =>
         val newSum: Long = (sum.toLong * 10L) + number.toLong
         if (newSum > Int.MaxValue || newSum < Int.MinValue) throw new java.lang.ArithmeticException("Int out of bounds")
         newSum.toInt
       }
        res * x.signum
     } catch {
       case e: java.lang.ArithmeticException => 0

     }


   }
  def reverse1(x: Int): Int = {
    val s = if (x < 0) x.toString.tail else x.toString
    val d = s.reverse.toDouble * x.signum
    if  (d.isValidInt) d.toInt else 0
  }

  def time[A](f:A):A={
    val start = System.nanoTime()
    val r = f
    println(System.nanoTime - start)
    r
  }
  println(time(reverse(12340)))
  println(time(reverse1(12340)))
  //println(time(reverse(1534236469)))
  //println(time(reverse1(1534236469)))
}
