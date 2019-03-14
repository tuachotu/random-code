/*Implement atoi which converts a string to an integer.

  The function first discards as many whitespace characters as necessary until the first non-whitespace character is found. Then, starting from this character, takes an optional initial plus or minus sign followed by as many numerical digits as possible, and interprets them as a numerical value.

  The string can contain additional characters after those that form the integral number, which are ignored and have no effect on the behavior of this function.

If the first sequence of non-whitespace characters in str is not a valid integral number, or if no such sequence exists because either str is empty or it contains only whitespace characters, no conversion is performed.

If no valid conversion could be performed, a zero value is returned.

 */
object AdvanceAtoi extends App{
  def removeSpaceFromFront(s: String): String = s.dropWhile(_==' ')
  def firstNumberInString(s:String): String = ("""^[-+]{0,1}\d+""".r findFirstIn s).getOrElse("0")

  def myAtoi(str: String): Int = {
    val number = firstNumberInString(removeSpaceFromFront(str)).toDouble
    if (number.isValidInt) number.toInt
    else if (number < 0) Int.MinValue
    else Int.MaxValue
  }

  println(myAtoi("   -123"))
  println(myAtoi("   123 345"))
  println(myAtoi("   -123 vikrant"))
}
