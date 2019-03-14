import scala.collection.mutable.Set
import scala.language.implicitConversions

object findCombination extends App {
  val size = args(0).toInt
  implicit def !(n:Int): Int = factorial(n)
  def factorial(n: Int): Int = (1 to n).foldLeft(1)(_*_)
  def countCombinations(n: Int, r: Int): Int = factorial(n)/(factorial(r)*factorial(n-r))
  def countVariations(n: Int, r: Int): Int = factorial(n)/factorial(n-r)

  def combinations(l: List[Int], n: Int): List[List[Int]] =
    if (n == 1 ) {
      l.map(List(_))
    } else {
      l match {
        case h :: t =>
         combinations(t, n - 1).map(h :: _) ++ combinations(t,n)
        case _ => List()
      }
    }

  def dropElement(l: List[Int], index: Int): List[Int] = l.take(index) ++ l.drop(index+1)
  def variations(l: List[Int], n: Int, variation: List[Int]): List[List[Int]] = {
    if (variation.length == n) {
      if (discovered contains variation.mkString("")) List()
      else { discovered += variation.mkString(""); List(variation) }
    } else {
      l.zipWithIndex flatMap { case (number, index) =>
        variations(dropElement(l, index), n, variation ++ List(number)) ++
          variations(dropElement(l, index), n, variation)
      }
    }
  }

//  println(countCombinations(List(4,4,2,5,6,7,8).length, size))
//  println(combinations(List(4,4,2,5,6,7,8), size).length)
  val discovered = Set[String]()
  variations(List(4,7,8), size, List()) foreach ( v => println(v.mkString(",")))
  println(countVariations(List(4,7,8).length, size))
}
