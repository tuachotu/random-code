

import scala.util.Random

object MatrixRotate extends App {

  def getCol(c: Int, m: Array[Array[Int]]): Seq[Int] = (0 to (m(0).length - 1)) map ( r => m(r)(c))
  def getRow(r: Int, m: Array[Array[Int]]): Seq[Int] = m(r).toSeq
  def innerMatrix(m: Array[Array[Int]]): Array[Array[Int]] = {
    if (m.length == 0) Array.empty
    else {
      val rows = m.length - 1
      val cols = m(0).length - 1

      (1 until rows).map { r =>
        (1 until cols).map { c =>
          m(r)(c)
        }.toArray
      }.toArray
    }
  }

  val rotatedMatrix = Array.fill(6)(Array.fill(5)(-1))
  def rotateByOne(m: Array[Array[Int]], startRow: Int, startCol: Int,
                  endRow: Int, endCol: Int): Unit = {
    m.size match {
      case 0 =>
      case _ =>
        getRow(0, m) match {
          case Nil =>
          case x => x.init.zipWithIndex foreach { case (e, index) => rotatedMatrix(startRow)(startCol+index+1) = e }
        }
        getCol(m(0).length - 1, m) match {
          case Nil =>
          case x => x.init.zipWithIndex foreach { case (e, index) =>  rotatedMatrix(startRow+index+1)(endCol) = e }
        }
        getRow(m.length - 1, m) match {
          case x :: Nil =>
          case x => x.tail.zipWithIndex foreach { case (e, index) => rotatedMatrix(endRow)(startCol+index) = e }
        }
        getCol(0, m) match {
          case x :: Nil =>
          case x => x.tail.zipWithIndex foreach { case (e, index) => rotatedMatrix(startRow+index)(startCol) = e }
        }
        rotateByOne(innerMatrix(m), startRow + 1, startCol + 1, endRow - 1, endCol - 1)
    }

  }


  def rotate(m: Array[Array[Int]]): Array[Array[Int]]  = {
    (0 until m.length).map { i =>
      (0 until m(0).length).map { j =>
        newValueAt(m, i, j)
      }.toArray
    }.toArray
  }

  def newValueAt(in: Array[Array[Int]], i: Int, j: Int): Int = {
    val s = in.size - 1
    if      (i >= j && s - i >  j) in(i + 1)(j) // 'b'
    else if (i <  j && s - i >= j) in(i)(j - 1) // 'l'
    else if (i <= j && s - i <  j) in(i - 1)(j) // 'a'
    else if (i >  j && s - i <= j) in(i)(j + 1) // 'r'
    else                           in(i)(j)     // 's'
  }

  val matrix = Array.fill(6)(Array.fill(5)(Random.nextInt(100)))
  matrix foreach(row => println(row.mkString(",")))

  println("----------")

  rotateByOne(matrix, 0, 0, 5, 4)
  rotatedMatrix foreach (row => println(row.mkString(",")))


  println("----------")


  rotate(matrix) foreach (row => println(row.mkString(",")))

}


