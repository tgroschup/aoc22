import scala.io.Source

trait Shape {
  val loseTo: Shape
  val winAgainst: Shape
  def score: Int
  def compare(other: Shape): Int = other match {
    case this.loseTo => 0
    case this.winAgainst => 6
    case _ => 3
  }
}
case object Rock extends Shape {
  override def score: Int = 1

  override val loseTo: Shape = Paper
  override val winAgainst: Shape = Scissors
}
case object Paper extends Shape {
  override def score: Int = 2

  override val loseTo: Shape = Scissors
  override val winAgainst: Shape = Rock

}
case object Scissors extends Shape {
  override def score: Int = 3

  override val loseTo: Shape = Rock
  override val winAgainst: Shape = Paper
}

object Shape {
  def apply(c: Char): Shape = c  match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors
  }

  def apply(other: Shape, c: Char): Shape = c match {
    case 'X' => other.winAgainst
    case 'Y' => other
    case 'Z' => other.loseTo
  }
}

object Day2 {
  def parseStrategy(name: String, useStragey: Boolean = false): Long =
    val file = Source.fromResource(name)

    val scores = for (line <- file.getLines()) yield
      val opponent = Shape(line.head)
      val hint = line.last
      val self = if(useStragey) Shape(opponent, hint) else Shape(hint)
      self.compare(opponent) + self.score

    scores.sum
  def example: Long = parseStrategy("day2/example.txt")
  def star1: Long = parseStrategy("day2/star1.txt")
  def star2: Long = parseStrategy("day2/star1.txt", useStragey = true)
}
