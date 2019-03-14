object maxRectangleInHistogram extends App {
  val histogram1 = List(1,8,6,2,5,4,8,3,7)
  val histogram2 = List(1,5,4,3)
  val histogram3 = List(3,1,2,4,5)



  def areaBetweenIndexes(l: List[Int], in1: Int, in2: Int): Int = Math.abs(in1-in2) * Math.min(l(in1), l(in2))
  def findIndexBoundary(l: List[Int], in: Int): (Int, Int) = {
    val left = l.indices.take(in).reverse.find(x => l(x) < l(in)).getOrElse(Int.MinValue)
    val right = l.indices.drop(in).find(x => l(x) < l(in)).getOrElse(Int.MaxValue)
     (left,right) match {
       case (Int.MinValue, Int.MaxValue) => (0, l.length-1)
       case (Int.MinValue, _) => (0, right-1)
       case (_, Int.MaxValue) => (left+1, l.length-1)
       case (_, _) => (left+1, right-1)
     }
  }

  //histogram.indices foreach(index => println(index + " -> " + findIndexBoundary(histogram, index)))
  val area1 = (histogram1.indices map { index =>
    val (l, r) = findIndexBoundary(histogram1, index)
    areaBetweenIndexes(histogram1,l,r)
  }).max

  println(area1)



  val area2 = (histogram2.indices map { index =>
    val (l, r) = findIndexBoundary(histogram2, index)
    areaBetweenIndexes(histogram2,l,r)
  }).max

  println(area2)

  val area3 = (histogram3.indices map { index =>
    val (l, r) = findIndexBoundary(histogram3, index)
    areaBetweenIndexes(histogram3,l,r)
  }).max

  println(area3)

}
