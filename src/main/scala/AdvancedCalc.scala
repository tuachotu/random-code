
object AdvancedCalc extends App {
  val OpenBracket: Char = '('
  val CloseBracket: Char = ')'

  def boundary(c: Char): Boolean = c == OpenBracket || c == CloseBracket

  // TODO: handle for spaces
  // TODO: How to handle -ve numbers

  // takes a expression without parens , gives result
  def evaluateString(s: String) = {
    if (s.isEmpty) 0 else {
      (s.split("-") map { subs =>
        (subs.split("\\+") map { adds =>
          (adds.split("/") map { divs =>
            divs.split("\\*").map(_.toInt).product
          }).reduceLeft { (left, right) => left / right }
        }).sum
      }).reduceLeft { (left, right) => left - right }
    }
  }

  // takes a expression , and breaks it on parens
  // (1+(111+(16/2))) -> List(, (, 1+, (, 111+, (, 16/2, ), , ), , ), )
  // BUG - check why it is adding empty string, which requires me to put a filter in end
  def splitString(s: String): List[String] =
    (s.indexWhere(c => boundary(c)) match {
      case -1 => List(s)
      case index => List(s take index) ++ List(s(index).toString) ++ splitString(s.substring(index + 1))
    }).filter(_.nonEmpty)


  def evaluateStringWithInParams(s: String): String = {
    splitString(s).foldLeft(List.empty[String]) { case (lookback, subExpression) =>
      if (subExpression.nonEmpty &&  subExpression.head != CloseBracket) {
        lookback ++ List(subExpression)
      } else {
        // find a closing paren, going backwards
        List(lookback.foldRight((Option.empty[String], false)) { case (element, (subExpression1, closeBracketFound)) =>
          if (closeBracketFound) { (subExpression1, false) } else {
            if (element == OpenBracket.toString) (subExpression1, false)
            else if (element == CloseBracket.toString) (subExpression1, true)
            else {
              subExpression1 match {
                case  None => (Some(evaluateString(element).toString), false)
                // check if this is needed. we will never come across two exp together
                case Some(exp) => (Some(evaluateString(element + exp).toString), false)

              }
            }
          }
        }._1.get)
      }
    }.head
  }

  def evaluateStringWithParams(expression: String): Int = {
    val firstOpenParenIndex = expression.indexWhere(c => boundary(c))
    val lastCloseParenIndex = expression.length - expression.reverse.indexWhere(c => boundary(c))

    val expBeforeFirstParen = expression take firstOpenParenIndex
    val expAfterFirstParen = expression drop lastCloseParenIndex
    val expInParens = expression.substring(firstOpenParenIndex, lastCloseParenIndex)

    evaluateString(expBeforeFirstParen + evaluateStringWithInParams(expInParens) + expAfterFirstParen)
  }

  println(evaluateStringWithParams("(1+(111+(16/2)))+22"))
  println(evaluateStringWithParams("10+(1+(111+(16/2)))+22"))
  println(evaluateStringWithParams("(1+(111+(16/2)))"))
  println(evaluateStringWithParams("(1+(111+(0*16/2)))"))

}
