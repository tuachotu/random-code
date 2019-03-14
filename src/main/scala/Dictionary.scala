object Dictionary extends App {

  class dictionary(size: Int) {
    val data = Array.fill(size)(List[(String,String)]())

    def printDictionary : Unit =  data.filter(_.nonEmpty) foreach {entry => println(entry mkString ",")}
    def myHash(s:String):Int = s.hashCode
    def getIndex(s: String) : Int = myHash(s) % size
    def set(key: String, value: String) : Unit =
      data(getIndex(key)) = (key,value)::(data(getIndex(key)) filter ( mem => mem._1 != key))

    def get(k: String): String = (data(getIndex(k)) find ( x => x._1 == k )).head._2

    def contains(k: String): Boolean = (data(getIndex(k)) find ( x => x._1 == k )).nonEmpty

  }

    val myDictionary = new dictionary(100)

    myDictionary.set("aaa","bbb")
    myDictionary.set("aaa","bbbbbb")
    myDictionary.set("ppp","qqq")
    myDictionary.printDictionary
    println(myDictionary.contains("aaa"))
    println(myDictionary contains "aa")
}
