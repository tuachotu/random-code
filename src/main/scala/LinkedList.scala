object LinkedList extends App {

  sealed trait LinkListNode {
    def next: LinkListNode
    def value: Int
    def isEmpty: Boolean

    def add(p: Int): LinkListNode = {
      if (isEmpty) ValidLinkListNode(p)
      else if (next.isEmpty) ValidLinkListNode(value, ValidLinkListNode(p))
      else ValidLinkListNode(value, next.add(p))
    }

    def printList: Unit = if (!isEmpty) {
      print(value + ",")
      next.printList
    }

    def reverse: LinkListNode = if (isEmpty) EmptyLinkListNode else next.reverse.add(value)

    def remove(p: Int): LinkListNode =
      if (isEmpty) EmptyLinkListNode
      else {
        if (value != p) ValidLinkListNode(value, next.remove(p))
        else next
      }

    def removeAll(p:Int): LinkListNode =
      if (isEmpty) EmptyLinkListNode
      else {
        if (value != p) ValidLinkListNode(value, next.removeAll(p))
        else next.removeAll(p)
      }
  }

  case object EmptyLinkListNode extends LinkListNode {
    def next: LinkListNode = ???
    def value: Int = ???
    def isEmpty: Boolean = true
  }

  case class ValidLinkListNode(v: Int, nextNode: LinkListNode = EmptyLinkListNode) extends LinkListNode {
    def next: LinkListNode = nextNode
    def value: Int = v
    def isEmpty: Boolean = false
  }


   val v = EmptyLinkListNode.add(1).add(5).add(2).add(3).add(4).add(5).add(5).add(5)
   v.printList
   println
   //v.reverse.printList
   //v.remove(7).printList
   //v.remove(5).printList
  v.removeAll(5).printList




}
