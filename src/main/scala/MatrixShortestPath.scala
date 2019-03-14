
import scala.collection.mutable.{Queue, Set}

object MatrixShortestPath extends App {

  val matrix: Array[Array[Int]] =
    Array(Array( 1, 0, 1, 1, 1, 1, 0, 1, 1, 1 ),
          Array( 1, 0, 1, 0, 1, 1, 1, 0, 1, 1 ),
          Array( 1, 1, 1, 0, 1, 1, 0, 1, 0, 1 ),
          Array( 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
          Array( 1, 1, 1, 0, 1, 1, 1, 0, 1, 0 ),
          Array( 1, 0, 1, 1, 1, 1, 0, 1, 0, 0 ),
          Array( 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ),
          Array( 1, 0, 1, 1, 1, 1, 0, 1, 1, 1 ),
          Array( 1, 1, 0, 0, 0, 0, 1, 0, 0, 1 ))

  matrix foreach (row => println(row mkString ","))

  case class Location(row: Int, col: Int)

  def neighbors(pos: Location) : List[Location] = {
    List(Location(pos.row-1, pos.col),
      Location(pos.row, pos.col-1),
      Location(pos.row+1, pos.col),
      Location(pos.row, pos.col+1),
      Location(pos.row-1, pos.col-1),
      Location(pos.row+1, pos.col+1),
      Location(pos.row-1, pos.col+1),
      Location(pos.row+1, pos.col-1)
    )
  }

  def inRange(pos: Location, m: Array[Array[Int]]): Boolean =
    (0 <= pos.row) && (pos.row < m.length) && (0<= pos.col) && (pos.col < m.head.length)

  def possibleMoves(m: Array[Array[Int]], pos: Location): List[Location] = {
      neighbors(pos) filter (p => inRange(p, m) && m(p.row)(p.col) == 1)
  }

  def distance(source: Location, destination: Location): Int = {
    val lookupTable = Queue[(Location, Int)]((source,0))
    val visited = Set[Location]()
    var notFound = true
    var distance = 0
    while (lookupTable.nonEmpty && notFound  ) {
      val pos = lookupTable.dequeue()
        if (pos._1 == destination) {
          distance = pos._2 ; notFound = false
        } else {
          possibleMoves(matrix, pos._1) foreach { move =>
              if (!visited.contains(move)) {
                println(pos,move)
                visited.add(move)
                lookupTable.enqueue((move, pos._2 + 1))
              }
          }
        }
    }
    distance
  }

  println(distance(Location(0,0), Location(3,4)))

}
