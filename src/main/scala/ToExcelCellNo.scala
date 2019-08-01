object ToExcelCellNo extends App {
  def codeToString(num: Int): String = (64 + num).asInstanceOf[Char].toString
  def toExcel(num: Int): String = num match {
    case 0 => ""
    case n if n < 0 => toExcel(Math.abs(n))
    case n if n <= 26 => codeToString(n)
    case _ => toExcel(num/26) + codeToString(num%26)
  }

  //1 to 30 foreach (num => println(toExcel(num)))
  println(toExcel(701))

}
