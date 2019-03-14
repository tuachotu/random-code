import scala.collection.mutable.HashMap

object NearestDistance extends App {
  case class EditDistance(remove: Int, replace: Int, insert: Int)
  def findEditDistance(s1: String, s2: String) : EditDistance = {
    val lookupTable = s2.foldLeft(HashMap[Char, Int]()) { (m, c) =>
      if (m contains c ) m += c -> (m(c) + 1) else m(c) = 1
      m
    }

    s1 foreach { c =>
      if (lookupTable contains c ) lookupTable += c -> (lookupTable(c) - 1) else lookupTable(c) = -1
    }

    val distance = lookupTable.foldLeft(EditDistance(0,0,0))  { case (e, (k,v)) =>
      v match {
        case 0 => e
        case d if d > 0 => EditDistance(e.remove+d, e.replace, e.insert)
        case d => EditDistance(e.remove, e.replace, e.insert+d)
      }
    }

    if (distance.remove >= distance.insert) EditDistance(distance.remove-distance.insert, distance.insert, 0 )
    else EditDistance(0, distance.remove, distance.insert-distance.remove)
  }
  println(findEditDistance("geek", "gesek"))
  println(findEditDistance("cat", "cut"))
  println(findEditDistance("sunday", "saturday"))
  println(findEditDistance("akash", "hsaka"))

}
