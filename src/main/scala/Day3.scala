import scala.io.Source

object Day3 {
  def score(c: Char): Int =
    if(c >= 'a') c - '`'
    else c - 'A' + 27
  def checkCompartments(name: String): Long =
    Source.fromResource(name)
      .getLines()
      .map(line => line.splitAt(line.length/2))
      .map{
        case (c1, c2) => c1.filter(c2.contains)
      }
      .map(s => score(s.head))
      .sum

  def checkElfTriples(name: String): Long =
    Source.fromResource(name)
      .getLines()
      .toList
      .sliding(3,3)
      .map {
        case r1 :: r2 :: r3 :: _ =>
          r1.filter(
            c => r2.contains(c) && r3.contains(c)
          )
      }
      .map(s => s.head)
      .map(score)
      .sum

  def example: Long = checkCompartments("day3/example.txt")
  def star1: Long = checkCompartments("day3/star1.txt")

  def example2: Long = checkElfTriples("day3/example.txt")
  def star2: Long = checkElfTriples("day3/star1.txt")

}
