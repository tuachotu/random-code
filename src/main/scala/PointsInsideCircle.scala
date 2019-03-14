object PointsInsideCircle extends App {
  def time[A](f: => A): A = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }

  case class Point(x: Int, y: Int)
  def isPointInsideCircle(p: Point, r: Int) : Boolean = {
    if ( p == Point(0,0) ) false
    else if (Math.pow(p.x, 2) + Math.pow(p.y, 2) > Math.pow(r,2)) false
    else true
  }

  val radius = args(0).toInt
  val count = time {(for {
    x <- -radius to radius
    y <- -radius to radius
  } yield (x,y)).foldLeft(0){(count, p) =>
    if (isPointInsideCircle(Point(p._1, p._2), radius))  count + 1  else  count
  }}

  println(s" radius = $radius, count of points =  ${count + 1 } ") // 4 sides and 1 center


 /* val count2 = time {
    val pointsOnAxis = radius * 4 // 4 axis
    val pointOnPositiveSide = (for {
      x <- 1 to radius
      y <- 1 to radius
    } yield (x,y)).foldLeft(0){(c, p) =>
      if (isPointInsideCircle(Point(p._1, p._2), radius)) c + 1  else c
    }
    pointsOnAxis + pointOnPositiveSide*4 + 1
  }
  println(s" radius = $radius, count of points =  $count2  ") // 4 sides and 1 center
  */

}
