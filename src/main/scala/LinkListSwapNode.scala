trait linkList {
  def value: Int
  def isEmpty: Boolean
  def next: linkList
  def toString: String
  def add(value: Int): linkList
}

case object EmptyNode extends linkList {
  override def value : Int = ???
  def next: linkList = ???
  override def isEmpty: Boolean = true
  def add(value: Int): linkList = ???
  override def toString: String = ???
}

case class Node(v: Int, n: linkList=EmptyNode) extends linkList {
  override def isEmpty: Boolean = false
  override def value: Int = v
  def next: linkList = n
  override def toString: String = {
    v.toString + " -> " + (if (n.isEmpty) "empty" else next.toString)
  }
  def add(v: Int): linkList = {
    if (next.isEmpty) Node(value, Node(v))
    else Node(value, next.add(v))
  }

}

object LinkListSwapNode extends App {
 val myLinkList = Node(1,Node(2, Node(3, Node(4, Node(5, Node(6))))))
  //println(myLinkList.toString)

  val l1 = Node(1,Node(2, Node(3, Node(4, Node(5, Node(6))))))
  val l2 = Node(1,Node(2, Node(3)))

  def toList(num: Int): linkList = {
    val lookup = scala.collection.mutable.Stack[Int]()
    var temp = num

    while (temp > 0) {
      lookup.push(temp%10)
      temp = temp/10
    }

    val node = Node(lookup.pop)
    var temp1: linkList = node
    while (lookup.nonEmpty) {
       temp1 = temp1.add(lookup.pop)
    }
    temp1
  }

  def toNumber(l: linkList): Int = {
    val lookup = scala.collection.mutable.Stack[Int]()
    var temp = l
    while (!temp.isEmpty) {
      lookup.push(temp.value)
      temp = temp.next
    }
    var sum = 0
    var pow = 0
    while (lookup.nonEmpty) {
      sum += lookup.pop * Math.pow(10,pow).toInt
      pow+=1
    }
    println(sum)
    sum
  }

  println(toList(toNumber(l1) + toNumber(l2)).toString)

  def reverseInPair(l: linkList): linkList = {
    if (l isEmpty) l
    else if (l.next.isEmpty) l
    else {
      Node(l.next.value, Node(l.value, reverseInPair(l.next.next)))
    }
  }

  //println(reverseInPair(myLinkList).toString)

}
