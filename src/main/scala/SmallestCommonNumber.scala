import scala.util.Random

object SmallestCommonNumber extends App {

  def timeIt[T](f:T):T = {
    val start = System.nanoTime
    val r = f
    val end = System.nanoTime() - start
    println("time taken is " + end.toString)
    r
  }

  def findSmallest(lists: List[List[Int]]): Int = lists.map(_.distinct).reduceLeft(_.intersect(_)).min


  def findSmallestFast(lists: List[List[Int]]): Int = {
    val size = lists.length
    val lookup = collection.mutable.HashMap[Int, Int]()
    lists.map(_.toSet) foreach { list =>
      list foreach { element => if (lookup contains element) lookup(element) = lookup(element) + 1
                                else lookup += (element -> 1)
      }
    }
    lookup.filter(_._2 == size).keySet.min
  }

  //  println(findSmallest(List(List( 1, 3, 5, 10, 20 ), List( 2, 4, 5, 10, 20), List(2, 4, 10, 20)))) // gives 10
  //  println(findSmallest(List(List( 1,1,1,1, 3, 5, 10, 20 ), List( 2, 4, 5, 10, 20), List(2, 4, 10, 20), List(20)))) // gives 20

  1 to 10 foreach { _ =>
    print("without map  -> ")
    println(timeIt(findSmallest(List(List.fill(1000)(Random.nextInt(10)),List.fill(1000)(Random.nextInt(10)),List.fill(1000)(Random.nextInt(10))))))

    print("with map  -> ")
    println(timeIt(findSmallestFast(List(List.fill(1000)(Random.nextInt(10)),List.fill(1000)(Random.nextInt(10)),List.fill(1000)(Random.nextInt(10))))))
    //Thread.sleep(1000)

    println
    println
  }
}
