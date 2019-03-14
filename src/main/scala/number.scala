object palindrome {
  def rev(number: Int, temp: Int): Int = {
    if (number == 0 ) temp
    else {
    val newTemp = temp*10 + number%10
    rev(number/10, newTemp)
    }
  }
  def main(args: Array[String]): Unit = {
    println(rev(1234,0))

  }
}
