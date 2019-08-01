
import java.util.concurrent

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}



object test extends App {
  def someFuncSeq(s: List[Int]): Seq[String] = {
    for {
      num <- s
      if num %2 == 0
      s = num.toString
    } yield s

  }

  def checkCond(s: List[Int]): Boolean = s.nonEmpty

  def someFuncFuture(s: Seq[Int]) = {
    for {
      nums <- Future(s)
    } yield  {
      if (checkCond(nums.toList)) {
        someFuncSeq(nums.toList)
      } else {
        Seq.empty
      }
    }
  }

  def checkCond(condition: Boolean)(fail: Exception): Future[Unit] =
    if (condition) Future.unit else Future(throw fail)

  def someFuncFuture2(s: Seq[Int]) = {
    for {
      nums <- Future(s)
      x <- checkCond(nums.isEmpty)(new Exception("Failed"))
    } yield someFuncSeq(nums.toList)
  }

  def printResult(f: Future[Seq[String]]): Unit = {
    println(Await.result(f, Duration.Inf))
  }

  def printResultSafe(f: Future[Seq[String]]): Unit = {
   Try(Await.ready(f, Duration.Inf)) match {
     case Success(x) => println(x)
     case Failure(e) => e.printStackTrace()
   }
  }

  //println(someFuncSeq(1 to 100 toList))
  //printResult(someFuncFuture(1 to 100 toList))
  //printResult(someFuncFuture(Seq.empty))
  //printResultSafe(someFuncFuture(Seq.empty))

  def someFutureOption(o: Option[Boolean]) = Future.successful(o)

//  def processFutureOption(o: Option[Boolean]): Future[String] = {
//    for {
//      boolOption <- someFutureOption(o)
//      if boolOption.isDefined
//      value = boolOption.get.toString
//    }  yield  value
//  }

    def processFutureOption(o: Option[Boolean]): Future[String] = {
      for {
        boolOption <- someFutureOption(o)
        value = if (boolOption.isDefined) Some(true) else None
      } yield value.toString
    }

  Seq(Some(true), Some(false), None) foreach { value =>
    Try(Await.ready(processFutureOption(value), Duration.Inf)) match {
      case Success(x) => println(x)
      case Failure(e) => e.printStackTrace()
    }
  }


Await.ready(Future.never, Duration.Inf)

}
