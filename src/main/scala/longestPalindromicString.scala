object longestPalindromicString extends App {
  def maxOfThree(a:Int, b: Int, c: Int) : Int = Math.max(a, Math.max(b,c))
  def longestAtMid(str: String) : String =  {
    if (str.length == 1) str else {
      var found:Boolean = true
      val l = str.length / 2
      var (left,right) = if ((str.length % 2) == 0 ) (l-1,l) else (l,l)
      println(left,right,str)
      var result: String = ""
      while (found && left >= 0 && right < str.length) {
      if (left == right) {
        if (left-1 >=0 && right+1 <= str.length-1 && str(left-1) == str(right+1)) {
          left -= 1
          right += 1
          result = str.substring(left,right+1)
        } else if (left > 0 && str(left-1) == str(right)) {
          left -= 1
          result = str.substring(left,right+1)
        } else if (right < str.length-1 && str(left) == str(right+1)) {
          right += 1
          result = str.substring(left,right+1)
        } else {
          result = str.substring(left,right+1)
          found = false
        }
      } else if (str(left) == str(right)) {

        result = str.substring(left,right+1)
        println(left,right,str, result)
        left -= 1
        right += 1
      } else  { result = str.substring(left,right); found = false}
    }
    result
  }}

  def longestPalindrome(s: String): String = {
    if (s isEmpty) ""
    else if (s.length == 1) s
    else {
      val mid = (s.length)/2
      val left = longestPalindrome(s take mid)
      val right = longestPalindrome(s drop mid)
      val middle = longestAtMid(s)

      if (left.length > right.length && left.length > middle.length) left
      else if (right.length > left.length && right.length > middle.length) right
      else middle
    }
  }

  //println(longestPalindrome("babad"))
  //println(longestPalindrome("cbbd"))
  println(longestPalindrome("ac"))
  //println(longestPalindrome("CCCC"))
  //println(longestPalindrome("a"))


}
