import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering


class MinHeap (numbers: ArrayBuffer[Int]) {
  private def leftChild(index: Int): Int = 2*index
  private def rightChild(index: Int): Int = 2*index + 1
  private def parent(index:Int): Int  = (index -1)/ 2

  override def toString: String = numbers.mkString(",")

  def enqueue(): Int = {1}
  def dequeue(): Int = {1}

  def minHeapify(pos: Int): Unit = {
    var smallest = pos
    if ((leftChild(pos) < numbers.length) && (numbers(leftChild(pos)) < numbers(smallest))) { smallest = leftChild(pos)}
    if ((rightChild(pos) < numbers.length) && (numbers(rightChild(pos)) < numbers(smallest))) { smallest = rightChild(pos)}

    if (smallest != pos) {
      val temp = numbers(smallest)
      numbers(smallest) = numbers(pos)
      numbers(pos) = temp
      minHeapify(smallest)
    }
  }
  def heapify(): Unit = ((((numbers.length)/2) -1) to 0 by -1) foreach { pos =>
    minHeapify(pos)
  }

}

object FindMLargestElement extends App {
  val input = ArrayBuffer.fill(10)(scala.util.Random.nextInt(100))
  println(input.mkString(","))

  val myMinHeap = mutable.PriorityQueue.empty(Ordering.Int.reverse)
  input foreach (myMinHeap.enqueue(_))
  println(myMinHeap.dequeueAll.mkString(","))

  val myHeap = new MinHeap(input)
  myHeap.heapify()
  println(myHeap.toString)
}