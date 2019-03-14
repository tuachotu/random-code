import util.control.Breaks._
import scala.math.Ordering._
import scala.math.Ordering
import Numeric.Implicits._
import Ordering.Implicits._

object NearestMatch {
 def minDif[N:Numeric](x :Seq[N] ,y :Seq[N] ,z :Seq[N]
                     ,curSet :Seq[N] ,curDiff :N) :Unit = {

  if (x.isEmpty || y.isEmpty || z.isEmpty)
    println(curSet.mkString(","))  //done
  else {
    val newSet  = Seq(x.head, y.head, z.head)
    val newMin  = newSet.min
    val newDiff = newSet.max - newMin
    val (nxtSet, nxtDiff) = if (curDiff > newDiff) (newSet, newDiff)
                            else                   (curSet, curDiff)
    newSet match {
      case Seq(`newMin`,_,_) => minDif(x.tail, y, z, nxtSet, nxtDiff)
      case Seq(_,`newMin`,_) => minDif(x, y.tail, z, nxtSet, nxtDiff)
      case Seq(_,_,`newMin`) => minDif(x, y, z.tail, nxtSet, nxtDiff)
    }
  }
}

  def min(a: Array[Int]): Int =  {
    val diff1 = Math.abs(a(0) - a(1))
    val diff2 = Math.abs(a(0) - a(2))
    val diff3 = Math.abs(a(2) - a(1))
    if (diff1 >= diff2 && diff1 >= diff3) diff1
    else if (diff2 >= diff1 && diff2 >= diff3) diff2
    else diff3
  }
  def main(args: Array[String]) = {
    val size = args(0).toInt
    val range = args(1).toInt
   //val (a,b,c) = (Array(3,10,15), Array(2,11,21), Array(9,11,111))
//   val (a,b,c) = (Array(3,10,15), Array(2,11,21), Array(1,7,21))
  // val (a,b,c) = (Array(1,5,10), Array(4,6,15), Array(7,8,16))
//   val (a,b,c) = (Array(2,5,10), Array(4,6,15), Array(7,8,16))
   val (a,b,c) = (Array.fill(size)(scala.util.Random.nextInt(range)).sorted,Array.fill(size)(scala.util.Random.nextInt(range)).sorted,Array.fill(size)(scala.util.Random.nextInt(range)).sorted)
   //println(a.mkString(","))
   //println(b.mkString(","))
   //println(c.mkString(","))

   val t0 = System.nanoTime()
   minDif(a,b,c,Seq(),12345)
   val t1 = System.nanoTime()
   println("Elapsed time recursive " + (t1 - t0) + "ns")
/*   val combinations = for {
     i <- a
     j <- b
     k <- c
   } yield Array(i,j,k)

   println(combinations.size)
   val min1 =  combinations.foldLeft((Array[Int]())) { (currentMin, combination) =>

     if (currentMin.isEmpty) combination else
     if (min(combination) < min(currentMin)) combination else currentMin
   }
   println(min1)
   */


  val t2 = System.nanoTime()
  var (i,j,k) = (0,0,0)
  var (pa,pb,pc) = (0,0,0)
  var curDiff = 12345
  while (i < a.size && j < b.size && k < c.size) {
    val min = Math.min(a(i), Math.min(b(j), c(k)))
    val max = Math.max(a(i), Math.max(b(j), c(k)))
    val newDiff = max-min
   // if (newDiff == 0) break
    if (curDiff > newDiff) {
      pa = i
      pb = j
      pc = k
      curDiff = newDiff
    }
    if (a(i) == min) i+=1
    else if (b(j) == min) j+=1
    else k+=1
  }
   println(a(pa), b(pb), c(pc))
   val t3 = System.nanoTime()
   println("Elapsed time iterative " + (t3 - t2) + "ns")

  }
}
