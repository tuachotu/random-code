import scala.collection.mutable._

object KnightMoves extends App {
  case class Pos(row: Int, col: Int)
  val Size = 4

  def calculateMoves(from: Pos, target: Pos ): (Int, Seq[Pos])= {
    val pendingPos = collection.mutable.Queue[Pos](from)
    val positionVisited = collection.mutable.HashMap[Pos, (Int, Seq[Pos])](from -> (0, Seq()))
    var targetReached = false

    while(pendingPos.nonEmpty && !targetReached) {
      val p = pendingPos.dequeue()
      possibleMoves(p) foreach { position =>
        if ( position == target) {
          targetReached = true
        } else if (!(positionVisited contains position)) {
          pendingPos enqueue position
        }
        positionVisited += position -> ((positionVisited(p)._1 + 1,(positionVisited(p)._2 ++ Seq(p))))
      }
    }
    if (targetReached) positionVisited(target) else (-1, Seq())
  }

  def isValidPos(position: Pos): Boolean =
    ((0 until Size) contains position.row) && ((0 until Size) contains position.col)

  def possibleMoves(position: Pos): List[Pos] =
    List(Pos(position.row - 2, position.col + 1),
        Pos(position.row - 2, position.col - 1),
        Pos(position.row + 2, position.col + 1),
        Pos(position.row + 2, position.col - 1),
        Pos(position.row - 1  , position.col + 2),
        Pos(position.row - 1  , position.col - 2),
        Pos(position.row + 1  , position.col + 2),
        Pos(position.row + 1  , position.col - 2)
    ) filter( pos => isValidPos(pos))

  println(calculateMoves(Pos(0,1),Pos(0,0)))
  println(calculateMoves(Pos(0,1),Pos(0,2)))
}
