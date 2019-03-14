import scala.collection.mutable.Set

object matrixIsland {
  val ROWS = 5
  val COLS = 5
  case class Element(r: Int, c: Int)
  //val matrix = Array(Array(1,0,0,1,0),Array(1,1,0,1,1),Array(1,0,1,1,0),Array(1,0,1,0,1),Array(1,1,1,1,0))
  //val matrix = Array(Array(1,0,1,0,0),Array(0,0,0,0,1),Array(0,1,0,0,0),Array(0,0,0,1,0),Array(0,1,0,0,0))
  val matrix = Array(Array(1,1,0,0,0),Array(0,1,0,0,1),Array(1,0,0,1,1),Array(0,0,0,0,0),Array(1,0,1,0,1))

  def neighboringElements(e: Element): Seq[Element] = {
    val neighbors = Seq(Element(e.r-1, e.c-1), Element(e.r-1, e.c), Element(e.r-1, e.c+1),
                        Element(e.r, e.c-1), Element(e.r, e.c+1),
                        Element(e.r+1, e.c-1), Element(e.r+1, e.c), Element(e.r+1, e.c+1))

    neighbors filter ( n => n.r >= 0 && n.r < ROWS && n.c >= 0 && n.c < COLS)
  }

  val elementVisited = Set[Element]()

  def visitNeighbors(e: Element) {
    neighboringElements(e) foreach { n =>
      if (!(elementVisited contains Element(n.r,n.c)) && (matrix(n.r)(n.c) == 1)) {
        elementVisited.add(n)
        visitNeighbors(n)
      }
    }
  }

  var count = 0

  def numberOfIslands(matrix: Array[Array[Int]]): Int = {
    var row = 0
    while (row < ROWS) {
      var col = 0
      while (col < COLS) {
        if (( matrix(row)(col) == 1 ) && !(elementVisited contains Element(row,col))) {
          println(s"Found a new island $row $col")
          count = count + 1
          elementVisited.add(Element(row,col))
          visitNeighbors(Element(row,col))
        } else {
          elementVisited.add(Element(row,col))
        }
        col = col + 1
      }
      row = row + 1
    }
    count
  }

  def main(args: Array[String]): Unit = {
    println(numberOfIslands(matrix))
  }
}
