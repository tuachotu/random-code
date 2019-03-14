import scala.collection.mutable.Stack

object Expression1 extends App {
  val operators = List("+", "*", "/", "-")

  def isOperator(s: String): Boolean = operators contains s

  def calculate(p: String, q: String, op: String): String = op match {
      case "+" => (p.toFloat + q.toFloat).toString
      case "*" => (p.toFloat * q.toFloat).toString
      case "/" => (p.toFloat / q.toFloat).toString
      case "-" => (p.toFloat - q.toFloat).toString
    }

  def calculateExpr(s: List[String]): Float = {
    s.zipWithIndex.find(x => isOperator(x._1)) match {
      case Some((op, index)) =>
        val res =  calculate(s(index-2), s(index-1), s(index))
        val newList = s.take(index-2) ++ List(res) ++ s.drop(index+1)
        calculateExpr(newList)
      case None => s.head.toFloat
    }
  }


  def calculateExprItr(words: List[String]): Float = {
    val lookup = Stack[String]()
    words foreach { word =>
      if (!isOperator(word)) lookup.push(word)
      else lookup.push(calculate(lookup.pop, lookup.pop, word))
    }
    lookup.pop.toFloat
  }


  println(calculateExprItr(List("1","2","+","5","*")))
  println(calculateExprItr(List("1","2","+","5","*")))
}
