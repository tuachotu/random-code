import scala.collection.immutable.List

object PanCakeProblem extends App {

  def flip(l: List[Int], i: Int): List[Int] = l.take(i+1).reverse ++ l.drop(i+1)

  def flipLargest(l:List[Int]): List[Int] =
    flip(flip(l, l.zipWithIndex.maxBy(_._1)._2), l.length - 1)

  def sortPencakes(l: List[Int]): List[Int] =
    (l.length to 1 by -1).toList.foldLeft(l) { case (pc, index) =>
      flipLargest(pc.take(index)) ++ pc.drop(index)
    }

  val pencakes = List.fill(6)(util.Random.nextInt(10))
  println(pencakes.mkString(","))

  println(sortPencakes(pencakes).mkString(","))



}
