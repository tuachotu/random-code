import TrieSol1.TrieNode

import scala.collection.mutable.HashMap

object TrieSol1 extends App {

  case class TrieNode(v: Option[Char], var isLast: Boolean, children: HashMap[Char, TrieNode])

  def insertIntoTrie(node: TrieNode, key: String): Unit = {
    key match {
      case "" => node.isLast = true
      case _ =>
        if (node.children.isEmpty || !(node.children contains key.head)) {
          val childNode = TrieNode(Some(key.head), false, HashMap[Char, TrieNode]())
          insertIntoTrie(childNode, key.tail)
          node.children += (key.head -> childNode)
        } else { insertIntoTrie(node.children(key.head), key.tail) }
    }
  }

  def printTrie(node: TrieNode): Unit = {
    if (node.v.isDefined) print(node.v.get) else print("/")
    if (node.isLast) println
    if (node.children.nonEmpty) {
      node.children foreach { case (k, v) =>  printTrie(v) }
    }
  }

  def findByPrefix(node: TrieNode , prefix: String, matchTillNow: Seq[Char] = Seq[Char]()): scala.collection.Seq[String] = {
    if (prefix.isEmpty) {
      if (node.children.isEmpty) Seq[String](matchTillNow.mkString(""))
      else node.children flatMap { case (k, v) => findByPrefix(v, prefix = "", matchTillNow :+ k) } toSeq
    } else {
      if (node.children.isEmpty || !(node.children.contains(prefix.head))) Seq[String]()
      else findByPrefix(node.children(prefix.head), prefix.tail, matchTillNow :+ prefix.head)
    }
  }

  def contains(word: String): Boolean = ???
  def remove(word: String): Boolean = ???

  val head = new TrieNode(v = None,  isLast = false, children = HashMap[Char,TrieNode]())
  insertIntoTrie(head,"vikrant")
  insertIntoTrie(head,"vikas")
  insertIntoTrie(head,"sundar")
  printTrie(head)
  //println(findByPrefix(head, prefix = "sun"))
  println(findByPrefix(head, prefix = "vikas"))
  //println(findByPrefix(head, prefix = "viks"))
}


