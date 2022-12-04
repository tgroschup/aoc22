import scala.io.Source

case class Interval(lower: Int, upper: Int):
  def contains(x: Int): Boolean =
    lower <= x && upper >= x

  def contains(other :Interval): Boolean =
    lower <= other.lower && upper >= other.upper
end Interval


object Interval:
  def apply(s: String) =
    val bounds = s.split("-")
    new Interval(
      lower = java.lang.Integer.parseInt(bounds(0)),
      upper = java.lang.Integer.parseInt(bounds(1)),
    )


object Day4:
  def fullyContained(a: Interval, b: Interval): Boolean =
    (a contains b) || (b contains a)

  def overlap(a: Interval, b: Interval): Boolean =
    (a contains b.lower) || (a contains b.upper) || (b contains a.lower) || (b contains a.upper)

  def count(assignments: List[(Interval, Interval)], predicate: (Interval, Interval) => Boolean): Int =
    assignments count(predicate(_, _))


  def readFile(name: String): List[(Interval, Interval)] =
    Source.fromResource(name)
      .getLines()
      .map(
        _.split(",")
        .map(Interval.apply)
        .toList
      )
      .map {
        case l1 :: l2 :: _ => (l1, l2)
      }
      .toList

  def example = count(readFile("day4/example.txt"), fullyContained)
  def star1 = count(readFile("day4/star1.txt"), fullyContained)

  def example2 = count(readFile("day4/example.txt"), overlap)
  def star2 = count(readFile("day4/star1.txt"), overlap)
