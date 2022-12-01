import scala.collection.mutable
import scala.io.Source

object Day1 {
  def readCalories(name: String, count: Int): Long = 
    val file = Source.fromResource(name)
    var currentElf = 1
    val elfCalMap = mutable.Map[Int, Long](1 -> 0)
    for (line <- file.getLines()) 
      line match 
        case "" =>
          currentElf += 1
          elfCalMap(currentElf) = 0
        case calorie =>
          val cal = java.lang.Long.parseLong(calorie)
          elfCalMap(currentElf) += cal
      
    elfCalMap.values.toSeq.sorted.takeRight(count).sum

  def example(): Long = readCalories("day1/example.txt", 1)
  def star1(): Long = readCalories("day1/star1.txt", 1)
  def star2(): Long = readCalories("day1/star1.txt", 3)
}
