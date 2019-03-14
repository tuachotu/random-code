import scala.language.postfixOps

object PrintSpreadSheet extends App {

  val validAlphabets = ('A' to 'Z').toSeq

  def cells(range: String): Seq[String] = {
    val corners = (range split ":") flatMap { corner =>
      Seq(corner.head, corner.last)
    }
    val rows = (corners filter (r => validAlphabets.contains(r))) sorted
    val cols = (corners filter (c => !validAlphabets.contains(c))) sorted

    (rows.head to rows.last) flatMap { r =>
      (cols.head to cols.last) map { c =>
        r.toString + ":" + c.toString
      }
    }
  }
  cells("A3:D5") foreach println
}
