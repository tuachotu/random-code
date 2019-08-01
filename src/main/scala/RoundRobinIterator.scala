
object RoundRobinIterator {
  def getNewList(lists: List[List[Int]]): List[List[Int]] = {
    lists.filter(_.nonEmpty) match {
      case Nil => Nil
      case ll: List[List[Int]] => ll.map(_.head) :: getNewList(ll.map(_.tail))
    }
  }
  def apply(lists: List[List[Int]]): RoundRobinIterator =
    new RoundRobinIterator(getNewList(lists).flatten.iterator)
}
class RoundRobinIterator(val i: Iterator[Int]) {
  def hasNext: Boolean = i.hasNext
  def getNext = i.next
}

object RoundRobinIteratorSol extends App {
  val l: List[List[Int]] = List(List(1, 5, 10), List(2, 6), List(), List(3))
  val myIterator = RoundRobinIterator(l)
  while(myIterator.hasNext) println(myIterator.getNext)

}
