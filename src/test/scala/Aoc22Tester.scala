import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Aoc22Tester extends AnyWordSpec with Matchers {
  "Day 1" must {
    "example" in {
      Day1.example() mustBe 24000
    }

    "star 1" in {
      Day1.star1() mustBe 69501
    }

    "star 2" in {
      Day1.star2() mustBe 202346
    }
  }

  "Day 2" must {
    "do example" in {
      Day2.example mustBe 15
    }

    "cal star 1 result" in {
      Day2.star1 mustBe 14531
    }

    "cal star 2 result" in {
      Day2.star2 mustBe 11258
    }
  }
}
