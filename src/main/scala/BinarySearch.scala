import scala.util.Random

object BinarySearch extends App {
  //val list = List.fill(10)(Random.nextInt(20)).sorted
  val list = List(1,2,3,4,5,6,7,8,9)

  def rotateBy(l:List[Int], count: Int): List[Int] = l.drop(count) ++ l.take(count)


  def findBinary(l: List[Int], v: Int): Option[Int] = {
    def findBinaryInternal(start: Int, end: Int): Option[Int] = {
      (start + end) /2 match {
        case _ if start > end => None
        case mid if l(mid) > v => findBinaryInternal(start,mid-1)
        case mid if l(mid) < v => findBinaryInternal(mid+1,end)
        case mid => Some(mid)
      }
    }
    findBinaryInternal(0,l.length-1)
  }


  def findInRotatedArray(l:List[Int], v: Int): Option[Int] = {
    def rotationPoint(index: Int): Boolean = index > 0 && l(index) < l(index -1)
    def findInRotatedInternal(start: Int, end: Int): Option[Int] = {
      (start + end)/2 match {
        case _ if start > end => println("rotationPoint found-3"); None
        case mid if l(mid) == v => Some(mid)
        case mid if rotationPoint(mid) && v > l(mid) => println("rotationPoint found");findInRotatedInternal(0, mid-1)
        case mid if rotationPoint(mid) && v < l(mid) => println("rotationPoint found-2");findInRotatedInternal(mid + 1, list.length-1)
        case mid if l(mid) > v => findInRotatedInternal(start,mid-1)
        case mid if l(mid) < v => findInRotatedInternal(mid+1,end)
      }
    }
    findInRotatedInternal(0, l.length-1)
  }

  println(findInRotatedArray(List(3,4,5,6,1,2),2))
  println(findInRotatedArray(List(3,4,5,6,1,2),1))
  println(findInRotatedArray(List(1,2,3,4,5,6),2))
  println(findInRotatedArray(List(1,2,3,4,5,6),5))
}
