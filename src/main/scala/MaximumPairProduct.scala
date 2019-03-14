import scala.util.Random

object MaximumPairProduct extends App {
  val numbers = Array.fill(10)(Random.nextInt(201) - 100 )

  val result = numbers.foldLeft((numbers(0), numbers(0), numbers(0), numbers(0))) {
    case ((maxPos, secondMaxPos, maxNeg, secondMaxNeg),  number)  =>
     if (number > maxPos) (number, secondMaxPos, maxNeg, secondMaxNeg)
     else if (number > secondMaxPos) (maxPos, number, maxNeg, secondMaxNeg)
     else if (number < maxNeg && number > secondMaxNeg ) (maxPos, secondMaxPos, number, secondMaxNeg)
     else if (number < secondMaxNeg) (maxPos, secondMaxPos, maxNeg, number)
     else (maxPos, secondMaxPos, maxNeg, secondMaxNeg)
  }

  println(numbers.mkString(","))
  println(result)

}
