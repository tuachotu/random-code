object MatrixPaths extends App {

  case class Position(x: Int, y: Int)

  def validPos(p: Position): Boolean = p.x < 3 && p.x >= 0 && p.y < 4 && p.y >= 0

  def nextPos(currentPos: Position): List[(Position, String)] =
    List((Position(currentPos.x + 1, currentPos.y), "R"), (Position(currentPos.x, currentPos.y + 1), "D")).filter(p => validPos(p._1))

  // should work from any "End" and Start
  val Start = Position(0, 0)
  val End = Position(2, 3)

  def paths(source: Position = Start, pathTillNow: List[String] = List.empty): List[String] = {
    nextPos(source) flatMap { pos =>
      pos._1 match {
        case End => List((pathTillNow ++ List(pos._2)).mkString("->"))
        case _ => paths(pos._1, pathTillNow ++ List(pos._2))
      }
    }
  }

  paths() foreach println
}

