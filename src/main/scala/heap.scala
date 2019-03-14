
import scala.collection.mutable._
object Heap {
  def apply(size: Integer, data: ArrayBuffer[Integer]): Heap = {
    new Heap(size, data)
  }
  def leftChild(n:Integer): Integer = 2 * n + 1
  def rightChild(n:Integer): Integer = 2 * n + 2
  def parent(n: Integer): Integer = (n-1)/2

}

class Heap(size: Integer, data: ArrayBuffer[Integer]) {
  import Heap._
  override def toString(): String = data.mkString(",")

  def swap(i: Int, j: Int): Unit = {
    val temp = data(i)
    data(i) = data(j)
    data(j) = temp
  }
  def heapifyMin(pos: Int, length: Int): Unit = {
    var  smallest = pos
    val l = leftChild(smallest)
    val r = rightChild(smallest)
    if (leftChild(pos) < length && data(smallest) >  data(l)) {
      smallest = l
    }
    if (rightChild(pos) < length  && data(smallest) >  data(r)) {
      smallest = r
    }
    if (smallest != pos) {
      swap(pos, smallest)
      heapifyMin(smallest, length)
    }
  }

  def heapify(pos: Int, length: Int): Unit = {
    var largest = pos
    val l = leftChild(largest)
    val r = rightChild(largest)
    if (leftChild(pos) < length && data(largest) < data(l)) {
      largest = l
    }
    if (rightChild(pos) < length  && data(largest) <  data(r)) {
      largest = r
    }
    if (largest != pos) {
      swap(pos, largest)
      heapify(largest, length)
    }
  }

  def getMax(): Integer = {1}
  def buildMaxHeap(): Unit = {
    var i = size/2 -1
    while ( i >= 0 ) {
      heapify(i, size)
      i = i - 1
    }
  }
  def buildMinHeap(): Unit = {
    var i = size/2 -1
    while ( i >= 0 ) {
      heapifyMin(i, size)
      i = i - 1
    }
  }


  // Assumes that we have a max heap
  def sort(length: Int) : Unit = {
    var  i = length;
    while ( i >= 0) {
      swap( 0, i)
      println(s"iteration $i, ${this.toString})")
      heapify(0,i)
       i = i -1
    }
  }
}

object HeapSort {
  def main(args: Array[String]): Unit = {
//    println("Heap Test")
//    val myHeap = Heap(10, ArrayBuffer(13,21,6,43,5,73,18,81,55,42))
//    println(myHeap.toString)
//    myHeap.buildMinHeap
//    println(myHeap.toString)
//    while(i < 7) {
//
//    }
    //myHeap.buildMaxHeap
    //println(myHeap.toString)
    /*myHeap.sort(9)
    println(myHeap.toString)
    */
  }

}

