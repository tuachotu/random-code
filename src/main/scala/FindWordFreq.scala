import collection.mutable.PriorityQueue
import collection.mutable.HashMap

object FindWordFreq extends App {
  case class wordFreq(w: String, c: Int) extends Ordered[wordFreq] {
    override def compare(that: wordFreq): Int = c compareTo(that.c)
  }
  val words = "aa bbb  ccc aa ddd aa ccc" split " +"
  val wordCount = HashMap[String, Int]()
  for  (word <- words)  {
    wordCount += (word -> (wordCount.getOrElse(word,0) + 1))
  }
  val myHeap = PriorityQueue[wordFreq]()
  wordCount.toSeq foreach { case (w,c) => myHeap.enqueue(wordFreq(w,c))  }
  println(myHeap.dequeueAll.map(_.w).mkString(","))
}
