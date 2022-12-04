import scala.io.Source

sealed trait Shape {
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
  def parseStrategy(name: String, useStrategy: Boolean): Long =
    Source.fromResource(name)
      .getLines()
      .map(_.toList)
      .map { case opponentMove :: _ :: hint :: _ =>
        val opponentShape = Shape(opponentMove)
        val ownShape = if (useStrategy) Shape(opponentShape, hint) else Shape(hint)
        ownShape.compare(opponentShape) + ownShape.score
      }
      .sum

  def example: Long = parseStrategy("day2/example.txt", useStrategy = false)

  def star1: Long = parseStrategy("day2/star1.txt", useStrategy = false)

  def star2: Long = parseStrategy("day2/star1.txt", useStrategy = true)
}
