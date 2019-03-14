object RotateMatrix extends App {
  def rotateBy90(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val rows = matrix.length
    val cols = matrix.head.length

    (0 until cols).toArray map { c =>
      ((0 until rows).toArray map { r =>
         matrix(r)(c)
      }).reverse
    }
  }

  rotateBy90(Array(Array(1,2,3), Array(5,6,7), Array(9,10,11))) foreach ( row => println(row.mkString(",")))
  //rotateBy90(Array(Array(1,2,3,4), Array(5,6,7,8), Array(9,10,11,12))) foreach ( row => println(row.mkString(",")))
  //rotateBy90(Array(Array(1,2),Array(3,4), Array(5,6) , Array(7,8), Array(9,10), Array(11,12))) foreach ( row => println(row.mkString(",")))

}
