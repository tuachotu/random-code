import util.control.Breaks._

object FindSplit extends App {
  def solution(a: Array[Int]): Boolean = {
      a.indices foreach { p1 =>
        val listAfterFirstDrop = a.take(p1) ++ a.drop(p1 + 1)
        listAfterFirstDrop.indices foreach { p2 =>
          val listAfterSecondDrop = listAfterFirstDrop.take(p2) ++ listAfterFirstDrop.drop(p2 + 1)
          if (listAfterSecondDrop.sum % 3 == 0) return true
        }
      }
    false
  }
  println(solution(Array(1,3,4,2,2,2,1,1,2)))
  println(solution(Array(1,2,1,2,1,2,1,2)))
}
