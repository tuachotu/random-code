object longestPalindromicString extends App {

  def longestAtMid(str: String) : String =  {
    if (str.length == 1)  {
         str
    } else {
      var found:Boolean = true
      val l = str.length / 2

      var (left,right) = if ((str.length % 2) == 0 ) (l-1,l) else (l,l)
      var result: String = ""
      while (found && left >= 0 && right < str.length) {
        if (str(left) == str(right)) {
          result = str.substring(left,right+1)
          left -= 1
          right += 1
        } else  {
          result = str.substring(left,right);
          found = false
        }

        if (left == 0 || right == str.length) found = false
    }
    result
  }}

  def longestPalindrome(s: String): String = {
    if (s isEmpty) ""
    else if (s.length == 1)  {
      s
    }
    else {
      val mid = (s.length)/2
      val left = longestPalindrome(s take mid)
      val right = longestPalindrome(s drop mid)
      val middle = longestAtMid(s)

      val s1 = (if (left.length > right.length && left.length > middle.length) left
      else if (right.length > left.length && right.length > middle.length) right
      else middle)

      println(s, s1)
      s1
    }
  }

  def lengthOfLongestSubstring(s: String): Int =
    longestPalindrome(s).size


  println(longestPalindrome("bcbb"))
  //println(longestPalindrome("cbbd"))
  //println(longestPalindrome("ac"))
  //println(longestPalindrome("CCCC"))
  //println(longestPalindrome("a"))


}
