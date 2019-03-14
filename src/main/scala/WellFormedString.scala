object WellFormedString extends App {
  println("hello world")

  def wellFormed(s: String): Boolean = {
    var keepScanning = true
    var count = 0

    for ( c <- s ) {
      c match {
        case '(' => count += 1
        case  ')' => count -= 1
        case _ =>
      }
    }
    count == 0
  }

  Seq("()()()", "(abc)", "((ab)c)(" ) foreach ( s => println(wellFormed(s)))
}
