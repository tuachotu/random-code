object ToExcelCellNo extends App {
  def codeToString(num: Int): String = (64 + num).asInstanceOf[Char].toString
  def toExcel(num: Int): String = num match {
    case 0 => ""
    case n if n < 0 => toExcel(Math.abs(n))
    case _ =>
      if (num <= 26) codeToString(num)
      else toExcel(num/26) + codeToString(num%26)
  }

  1 to 30 foreach (num => println(toExcel(num)))

}
