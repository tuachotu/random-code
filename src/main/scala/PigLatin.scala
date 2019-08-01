object PigLatin extends App {

  val vowels: Seq[Char] = Seq('a', 'e', 'o', 'i', 'u')

  def isAlphabet(c: Char): Boolean = (c <= 'z' && c >= 'a') || (c <= 'Z' && c >= 'A')

  def isDigit(c: Char): Boolean = c <= '0' && c >= '9'

  def firstVowel(word: String): Int = word.indexWhere(vowels contains _)

  def isCapital(c: Char): Boolean = c <= 'Z' && c >= 'A'

  // "everybody!!" -> ("everybody", "!!")
  // "oranges." -> ("oranges", ".")
  def punctuation(word: String): (String, String) = {
    if (!(isAlphabet(word.last) || isDigit(word.last))) {
      val res = punctuation(word.init)
      (res._1, res._2 + word.last)
    } else (word, "")
  }

  // "hello", 1 -> elloh
  // "hello", 2 -> llohe
  def shiftWordFrom(word: String, index: Int): String = (word drop index) + (word take index)

  def translate(s: String, suffix: String = "ay"): String =
    s.split(" +") map { word =>
      val (head, punct) = punctuation(word)
      val result = shiftWordFrom(head, firstVowel(head)) + suffix + punct
      if (isCapital(word.head)) result.head.toUpper + result.tail.toLowerCase else result
    } mkString " "

  Seq("Vikrant", "hello", "hello everybody!!", "Hey, its nice to see you!", "'A cat likes Apples, Bananas and Oranges.") foreach { w =>
    println(w + " -> " + translate(w))
  }
}
