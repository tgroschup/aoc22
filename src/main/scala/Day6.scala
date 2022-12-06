import scala.io.Source

object Day6:
  def searchFirstDifferenctCharacters(name: String, count: Int): Int =
    Source.fromResource(name)
      .sliding(count)
      .zipWithIndex
      .find {
        case (array, _) =>
          array.distinct.size == count
      }
      .getOrElse(throw Exception("no start marker found"))
      ._2 + count

  def example = searchFirstDifferenctCharacters("day6/example.txt", 4)
  def star1 = searchFirstDifferenctCharacters("day6/star1.txt", 4)

  def example2 = searchFirstDifferenctCharacters("day6/example.txt", 14)

  def star2 = searchFirstDifferenctCharacters("day6/star1.txt", 14)

