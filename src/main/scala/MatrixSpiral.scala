import scala.util.Random

object MatrixSpiral extends App {
  val matrix = Array.fill(6)(Array.fill(4)(Random.nextInt(10)))
  matrix foreach (row => println(row.mkString(",")))

  var rows = 6
  var cols = 5
  var r = 0
  var c = 0
  var startR = 0
  var startC = 0
/*
  println
  while (r < rows && c < cols) {
    for (i <- c until cols) {
      c = i; print(matrix(r)(i) + " ")
    }
    r += 1
    for (j <- r until rows) {
      r = j; print(matrix(j)(c) + " ")
    }
    c -= 1
    for (k <- c to startC by -1) {
      c = k; print(matrix(r)(k) + " ")
    }
    r -= 1
    for (l <- r to startR + 1 by -1) {
      r = l; print(matrix(l)(c) + " ")
    }

    rows -= 1
    cols -= 1
    c += 1
    startR = r
    startC = c
  }

*/
  def getRow(row: Int, m: Array[Array[Int]]): Seq[Int] = m(row)
  def getCol(col: Int, m: Array[Array[Int]]): Seq[Int] = (0 until m.length).map(i => m(i)(col)).toSeq
  def printNonEmpty(s: Seq[Int]) : Unit = if (s.isEmpty) print("") else print(s.mkString(","))

  def dropOuter(m: Array[Array[Int]]): Array[Array[Int]] = {
    val rows = m.length -1
    val cols = m(0).length -1
    (1 until rows).map { r =>
      (1 until cols).map { c =>
        m(r)(c)
      }.toArray
    }.toArray
  }

  def printSpiral(m: Array[Array[Int]]) : Unit  ={
    m.size match {
      case 0 => print("")
      case _=>
        printNonEmpty(getRow(0, m));print(",")
        if (m(0).length > 0 ) {
          printNonEmpty(getCol(m(0).length -1, m).tail);print(",")
        }
        val bottom = getRow(m.length - 1, m)
        if (m.length > 0 && bottom.nonEmpty) {
          printNonEmpty(bottom.init.reverse);print(",")
        }
        if (m(0).size > 1) {
          val left = getCol(0, m).init
          if (left.tail.nonEmpty) {
            printNonEmpty(left.tail.reverse);
            print(",")
          }
        }
        printSpiral(dropOuter(m))
    }
  }

  println("==================")
  printSpiral(matrix)


}


