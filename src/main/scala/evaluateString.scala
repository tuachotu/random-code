import scala.annotation.tailrec

object Expression extends App {


  def evaluateExpression(s :List[Char]): Int = {
    s match {
      case left::op::List(right) =>  // a+b or a*b
        op match {
          case '+' =>  left.asDigit + right.asDigit
          case  _ =>   left.asDigit * right.asDigit
        }
      case left::op::right => // a+b*c or a*b+c
         op match {
           case '+' =>
             left.asDigit + evaluateExpression(right)
           case '*' =>
             val nextAdd = right indexOf '+'
             val (multiplication ,pendingExpression) = if (nextAdd == -1) {
                                                         (right,List[Char]())
                                                       } else {
                                                         (right.take(nextAdd), right.drop(nextAdd + 1))
                                                       }
             val product = (multiplication.filter( _ != '*') map (_.asDigit)).foldLeft(left.asDigit) (_ * _)
             pendingExpression match {
               case Nil => product
               case head::Nil => product + head.asDigit
               case _ => product + evaluateExpression(pendingExpression)
            }
      }
    }
  }
  def time[A](f:A): A = {
    val t = System.nanoTime
    val r = f
    println(s"time = " + (System.nanoTime-t) + " ns")
    r
  }

  def evaluateExpression1(s :String) :Int =
  s.split("\\+")
   .map(_.split("\\*")
         .map(_.toInt)
         .product)
   .sum

//  List("3*3+3", "3*3*3", "2+2*3", "0*0*1", "0+0*1", "0*0*1", "2+2*3*3", "2+2+2*3+2+2") foreach { expr =>
  List("3*3+3+3*3*3+2+2*3*0*0*1*0+0*1+0*0*1+2+2*3*2+2+2*3+2+2") foreach { expr =>
    println(expr + " = " + time {evaluateExpression(expr.toList)})
    println(expr + " = " + time {evaluateExpression1(expr)})
  }
}
