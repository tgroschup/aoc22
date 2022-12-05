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

  "Day 3" must {
    "do example" in {
      Day3.example mustBe 157
    }

    "calculate star 1" in {
      Day3.star1 mustBe 7766
    }

    "do example 2" in {
      Day3.example2 mustBe 70
    }

    "calculate star " in {
      Day3.star2 mustBe 2415
    }
  }

  "Day 4" must {
    "example 1" in {
      Day4.example mustBe 2
    }

    "star 1" in {
      Day4.star1 mustBe 547
    }

    "example 2" in {
      Day4.example2 mustBe 4
    }

    "star 2" in {
      Day4.star2 mustBe 843
    }
  }

  "Day 5" must {
    "example 1" in {
      Day5.example mustBe "CMZ"
    }

    "star 1" in {
      Day5.star1 mustBe "BSDMQFLSP"
    }

    "example 2" in {
      Day5.example2 mustBe "MCD"
    }

    "star 2" in {
      Day5.star2 mustBe "PGSQBFLDP"
    }
  }
}
