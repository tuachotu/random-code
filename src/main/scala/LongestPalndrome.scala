object LongestPalndrome extends App {

  def palindormLengthAroundCenter(s: String) : Int = {
    val mid = s.length / 2

    // check around center
    var left = mid
    var right = mid
    var keepLooking = true
    var palindormLength = 1

    while(keepLooking && (left >= 0 || right < s.length )) {
      if (s(left) == s(right)) {
        palindormLength = Math.max((right - left + 1), palindormLength)
        left -= 1
        right += 1
      } else keepLooking = false
    }

    println(palindormLength)
    keepLooking = true

    left = mid-1
    right = mid
    println(s"comparing ${s(left)} vs ${s(right)}")
    while(keepLooking && (left >= 0 || right < s.length )) {
      if (s(left) == s(right)) {
        palindormLength = Math.max((right - left + 1), palindormLength)
        left -= 1
        right += 1
      } else keepLooking = false
    }

    palindormLength



  }

  //println(palindormLengthAroundCenter("vikrbabantp"))
  println(palindormLengthAroundCenter("vikabaabantp"))



}
