import scala.collection.mutable.Map

object MaxPopulation {
  case class years(birth: Int, death: Int)
  val input: Seq[years] = Seq(years(1962, 2015), years(1967, 2005), years(1912, 1975) , years(1902, 1999), years(1942, 2000))

  def main(args: Array[String]): Unit = {
    val yearMap = Map.empty[Int, Int].withDefaultValue(0)

    input foreach { person =>
      for {
        alive <- person.birth until person.death
      } yearMap(alive) += 1

      //if (yearMap(person.death) > 0) yearMap(person.death) -= 1
       yearMap(person.death) -= 1
    }
    val max = yearMap.maxBy {case (key, value) => value}
    println(max)
  }
}
