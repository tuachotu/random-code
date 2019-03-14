object GroupDuplicate extends App {
  //val words = List("ab", "ba", "tokyo", "kyoto", "paris", "rispa", null, "", "aaa")
  val words = List("ab", "ba", "tokyo", "kyoto", "paris", "rispa",  "aaa")

  def rotation(s1: String, s2: String): Boolean = (s1 + s1).indexOf(s2) != -1

  def findRotations(list: List[String], word: String): Set[String] =
    (list filter (mem => rotation(mem, word))).toSet

  def removeRotations(list: List[String], word: String): List[String] =
    list.filter(mem => !rotation(mem, word))

  def groupByRotatedWord(input: List[String]): List[Set[String]] =
    input match {
      case head::Nil => List(Set(head))
      case head::tail => List(findRotations(input, head)) ++
                         groupByRotatedWord(removeRotations(input, head))
      case Nil => List()
    }
   groupByRotatedWord(words.filter(_!=null)) foreach (w => println(w.mkString(",")))

}
