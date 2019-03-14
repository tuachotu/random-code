object FindPossibleIps extends App {

  def isValidIP(s: String): Boolean = {
    var valid = true
    val numbers = s split '.' map (_.toInt) iterator

    while (valid && numbers.nonEmpty) {
      valid = (numbers next) < 255
    }

    valid
   }

  def findValidBlocksAndPendingELements(s: String): List[(String,String)] = {
    (for {
      i <- 1 to 3
    } yield (s take i, s drop i)).toList
  }

  def enumerateIps(s: String): List[String] = {
    val ips = for {
      (a,p) <- findValidBlocksAndPendingELements(s)
       if p.nonEmpty
      (b,q) <- findValidBlocksAndPendingELements(p)
      if q.nonEmpty
      (c,r) <- findValidBlocksAndPendingELements(q)
      if r.nonEmpty
      (d,s) <- findValidBlocksAndPendingELements(r)
    } yield a + "." + b + "." + c + "." + d
    ips filter (_.length == (s.length + 3)) filter (isValidIP(_))
  }

 //enumerateIps("2234234234") foreach println
  enumerateIps("777234234234") foreach println





}
