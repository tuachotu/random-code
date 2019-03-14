import com.sun.tools.jdeps.Analyzer.Visitor

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue, Stack}

object Graph extends App {

  // Graph is adjancy matrix. node to connections
  //input given as (a,b)(a,c)(a,d)(b,c)(d,c)(c,e)
  case class Edge(v1: Char, v2: Char)


  def readGraph(edges: List[Edge]): HashMap[Char, ArrayBuffer[Char]] = {
    edges match {
      case Nil => HashMap[Char, ArrayBuffer[Char]]()
      case _ =>
        edges.foldLeft(HashMap[Char, ArrayBuffer[Char]]()) { (adjList, edge) =>
          if (adjList contains edge.v1) adjList(edge.v1) += edge.v2 else adjList += (edge.v1 -> ArrayBuffer(edge.v2))
          if (adjList contains edge.v2) adjList(edge.v2) += edge.v1 else adjList += (edge.v2 -> ArrayBuffer(edge.v1))
          adjList
        }
    }
  }

  def drawGraph(adjList: HashMap[Char, ArrayBuffer[Char]]): Unit = {
    adjList foreach { case (k, v) => println(s"$k -> ${v.mkString(",")}") }
  }


  def BFT(from: Char, adjList: HashMap[Char, ArrayBuffer[Char]]): Unit = {
    val visitedNodes = HashSet[Char]()
    val pendingNodes = Queue[Char]()

    pendingNodes.enqueue(from)

    while (pendingNodes.nonEmpty) {
      val nodeToVisit = pendingNodes.dequeue()
      if (!(visitedNodes contains nodeToVisit)) {
        visitedNodes += nodeToVisit
        adjList(nodeToVisit) foreach (neighbor => pendingNodes enqueue neighbor)
        println(nodeToVisit)
      }
    }
  }

  def DFT(from: Char, adjList: HashMap[Char, ArrayBuffer[Char]]): Unit = {
    val visitedNodes = HashSet[Char]()
    val pendingNodes = Stack[Char]()

    pendingNodes.push(from)

    while (pendingNodes.nonEmpty) {
      val nodeToVisit = pendingNodes.pop()
      if (!(visitedNodes contains nodeToVisit)) {
        visitedNodes += nodeToVisit
        adjList(nodeToVisit) foreach (neighbor => pendingNodes push neighbor)
        println(nodeToVisit)
      }
    }
  }

  // simple graph
  // val myGraph = readGraph(List(Edge('a', 'b'), Edge('a', 'c'), Edge('a', 'd'), Edge('b', 'c'), Edge('d', 'c'), Edge('c', 'e')))
  // Graph with many levels
  val myGraph = readGraph(List(Edge('s', '1'), Edge('s', '2'), Edge('1','3'), Edge('1','4'), Edge('1','5'), Edge('2','6'), Edge('6','7')))
  drawGraph(myGraph)
  println("-----------------------")
  BFT('s', myGraph)
  println("-----------------------")
  DFT('s', myGraph)
}