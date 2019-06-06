object QueenAttack extends  App {
  val r = 4
  val c = 4

  def isQueen(x: Int, y: Int): Boolean = (x == r) && (y == c)
  def isQueenAttacking(x: Int, y: Int): Boolean = (x == r) || (y == c) || (x + y) == (r +c) || (x-y) == (r-c)

  0 to 8 map { row =>
    0 to 8 map { col =>
      if (isQueen(row, col)) print(" Q")
      else if (isQueenAttacking(row, col)) print(" X")
      else print(" _")
    }
    println
  }

}
