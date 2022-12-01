import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Aoc22Tester extends AnyWordSpec with Matchers {
  "Day 1" when {
    "exampele" in {
      Day1.example() mustBe 24000
    }

    "star 1" in {
      Day1.star1() mustBe 69501
    }

    "star 2" in {
      Day1.star2() mustBe 202346
    }
  }
}
