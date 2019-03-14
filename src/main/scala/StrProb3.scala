/*
Count substrings that starts with character X and ends with character Y
Given a string str of n lowercase characters,
the task is to count the number of substrings of str starting with character X and ending with character Y.


Examples:

Input: str = "abbcaceghcak"
        x = 'a', y = 'c'
Output: 5
abbc, abbcac, ac, abbcaceghc, aceghc

Input: str = "geeksforgeeks"
        x = 'g', y = 'e'
Output: 6
 */

/*

Given a binary string, count number of substrings that start and end with 1.
For example, if the input string is “00100101”, then there are three substrings “1001”, “100101” and “101”.
 */
object StrProb3 extends App {
  def countStringsWithCorners(s: String, l: Char, r: Char): Int = {
    val (__, count) = s.foldLeft((0,0)) { case ((l_count, r_count), c) =>
      (if (c == l) l_count + 1 else l_count, if (c == r) l_count + r_count else r_count)
    }
    count
  }

  println(countStringsWithCorners("abbcaceghcak", 'a', 'c'))
  println(countStringsWithCorners("00100101",'1','1'))

}
