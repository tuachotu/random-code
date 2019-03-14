import scala.collection.mutable.ArrayBuffer

object MergeLists {
  def time[A](f: => A): A = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }

  def merge(list1: Array[Int], list2: Array[Int]): Array[Int]  = {
    if (list1 isEmpty) list2
    else if (list2 isEmpty) list1
    else {
      var i,j = 0;
      val finalList = ArrayBuffer[Int]()
      while (i < list1.size && j < list2.size) {
        if (list1(i) < list2(j)) {
          finalList += list1(i)
          i += 1
        } else {
          finalList += list2(j)
          j += 1
        }
      }
      if (i != list1.size) finalList ++= list1.slice(i, list1.size)
      if (j != list2.size) finalList ++= list2.slice(j, list2.size)
      finalList.toArray
    }
  }
  def main(args: Array[String]): Unit = {
    val r = scala.util.Random
    val l1 = Array.fill(10)(r.nextInt(100)).sorted
    val l2 = Array.fill(10)(r.nextInt(100)).sorted
    println(s"l1 = ${l1.mkString(",")}")
    println(s"l2 = ${l2.mkString(",")}")
    val mergedArray =  time { merge(l1,l2) }
    println(s"mergedArray = ${mergedArray.mkString(",")}")
  }
}
