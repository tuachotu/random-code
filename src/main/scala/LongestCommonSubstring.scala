object LongestCommonSubstring extends App {

  def longestCommonSubString(s1: String, s2: String): Int = {
    val lookupTable = Array.ofDim[Int](s1.length+1, s2.length+1)
    // LCSs for null string is size zero

    for (i <- 1 until s1.length +1) {
      for (j <- 1 until s2.length + 1) {
        if (i == 0 && j == 0) lookupTable(0)(0) = 0
        else if (s1(i-1) == s2(j -1))  lookupTable(i)(j) = (1 + lookupTable(i-1)(j-1))
        else lookupTable(i)(j) = 0
      }
    }

    lookupTable foreach (row => println(row.mkString(",")))
    1
  }

  longestCommonSubString("bcd", "abcd")
println
  longestCommonSubString("bcde", "abcdpde")

}
