object AnagramOfaPalindrom extends App {

  def isPalindrome(s: String) : Boolean = {
    if (s.length == 1) true
    else if (s.isEmpty) false
    else (s.head == s.last) && isPalindrome(s.tail.init)
  }

  println(isPalindrome("malayalam"))
}
