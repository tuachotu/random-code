/*
Given two non-negative integers num1 and num2 represented as strings, return the product of num1 and num2,
also represented as a string.

Example 1:

Input: num1 = "2", num2 = "3"
Output: "6"
Example 2:

Input: num1 = "123", num2 = "456"
Output: "56088"
Note:

The length of both num1 and num2 is < 110.
Both num1 and num2 contain only digits 0-9.
Both num1 and num2 do not contain any leading zero, except the number 0 itself.
You must not use any built-in BigInteger library or convert the inputs to integer directly.

 */
object StringMultipication extends App {
  def stringToNum(s: String): BigInt =
    s.reverse.zipWithIndex.foldLeft(BigInt(0)) { case (num, (digit, index)) =>
      num + BigInt(digit - '0') * BigInt(math.pow(10, index).toLong)
    }

  def numToString(num: BigInt): String = {
    if (num == 0 ) "0"
    else {
      var temp = num
      var res = ""
      while (temp > 0) {
        res = res + (temp % 10).toLong
        temp = temp / 10
      }
      res.reverse
    }
  }


  def multiply(num1: String, num2: String): String = {
    numToString(stringToNum(num1) * stringToNum(num2))
  }

  //println(numToString(BigInt(498828660196L)))
  //println(numToString(BigInt(840477629533L)))
  println(multiply("498828660196", "840477629533"))
  //println(multiply("12345", "2"))*/
}
