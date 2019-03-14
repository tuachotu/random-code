/*Given two strings, we need to take the character which has the maximum occurrence in the first string
 and then we have to check if that particular character is present in the second string the same number
 of times as it is present in the first string.
 */
import scala.language.postfixOps

object CharacterCountMatch extends App {

  val s1 = "sssgeek"
  val  s2 = "geeksss"

  val (maxChar,count) =
    ((s1.groupBy(x => x) map { case (c, cc) => c -> cc.length}).toSeq.sortBy{case (c, cnt) =>cnt}.reverse.head)
  val newLength = s2.filter(_==maxChar).length

  println(count, newLength)

}
