object findthestring {
 // following method is not needed for this problem
 def findNearestAndHigherPowOf2(n: Int) : Int= {
    var found = false
    var num = n
    if ( n == 0 || n == 1 ) 2
    else {
      while (!found) {
        if ( (num & num +1) == 0 ) found = true else num = num +1
      }
      num + 1
    }
  }

  def findNearestAndLowerPowOf2(n: Int) : Int= {
    var found = false
    var num = n
    if ( n == 0 || n == 1) 0
    else {
      while (!found) {
        if ( (num & num -1) == 0 ) found = true else num = num -1
      }
      num
    }
  }

  def findBinaryString(n : Int) : String = {
    if (n == 0) {
      ""
    } else if (!n.toBinaryString.contains('0')) {
      "0" * n.toBinaryString.size
    } else {
      val sizeOfString = (findNearestAndLowerPowOf2(n).toBinaryString).size
      val wothoutPadding = (n - (findNearestAndLowerPowOf2(n) -1)).toBinaryString
      ("0"* (sizeOfString - wothoutPadding.size -1)) + wothoutPadding
    }
  }

  def  main(args: Array[String]) : Unit =  {
    for (i <- 0 to 15)
      println(s" $i -> ${findBinaryString(i)}")
  }
}
