object Skyline extends App {
  case class Building(left: Int, right: Int, height: Int) {
    def topCorners: List[Point] = List(Point(left, height), Point(right, height))
  }

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    override def compare(that: Point): Int = that.x.compareTo(x)
  }
  val b : List[Building] = List(Building(2,9,10), Building(3,7,15),
                                        Building(5,12,12),Building(15, 20, 10),
                                        Building(19,24,8) )



  def overlappingCorners(leftBuilding: Building, rightBuilding: Building): List[Point] = {
    if (rightBuilding.left <= leftBuilding.right) {
      List(Point(leftBuilding.left, leftBuilding.height),
           Point(rightBuilding.left, Math.max(leftBuilding.height, rightBuilding.height)),
           Point(rightBuilding.right, rightBuilding.height))
    } else {
      List(Point(leftBuilding.left, leftBuilding.height),Point(leftBuilding.right, leftBuilding.height),
           Point(rightBuilding.left, rightBuilding.height), Point(rightBuilding.right, rightBuilding.height))
    }
  }

  def addToSkyLine(currentSkyline: List[Point], p: Point): List[Point] = {
    // look for x and then choose max Y
    currentSkyline.filter(pt => pt.x == p.x) match {
      case Nil => currentSkyline ++ List(p)
      case x => currentSkyline.filter(pt => pt.x != p.x) ++ List(Point(x.head.x, Math.max(x.head.y, p.y)))
    }
  }

  def skyLine(buildings: List[Building]): List[Point] = {
    buildings.sliding(2).foldLeft(List[Point]()) { case (result, List(leftB, rightB)) =>
      println(result, leftB, rightB)
      overlappingCorners(leftB, rightB).foldLeft(result)  { case (res, skyLinePoint) => addToSkyLine(res, skyLinePoint)}
    }
  }

  skyLine(b) foreach println
}
