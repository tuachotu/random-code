import scala.util.Random

object NeighborMultiplication extends App {
  val numbers = List.fill(10)(Random.nextInt(10))

  println(numbers mkString ",")

  def multiplication(l: List[Int], carryOver: Int = 1, useCarryOver: Boolean = false ): List[Int] = l match {
    case Nil => List()
    case x::Nil => List(carryOver * x)
    case x::y::Nil => List(carryOver * x * y, y * x)
    case x::y::z::Nil => List(carryOver * x * y, x * z, y * z)
    case x::y::z::tail =>
      if (useCarryOver) List(carryOver * y, x * z, y * tail.head) ++ multiplication(tail, z, true)
      else List(x * y, x * z, y * tail.head) ++ multiplication(tail, z, true)
  }


  println(multiplication(numbers).mkString(","))


}
