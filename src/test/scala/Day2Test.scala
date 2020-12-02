import org.scalatest.FlatSpec

import scala.io.Source

class Day2Test extends FlatSpec {

  it should "count As" in {
    val input = "aaa"
    assert(Day2.countLetters('a', input).equals(3))
  }

  it should "parse Input" in {
    val input = "1-3 a: aaa"
    assert(Day2.checkOnePw(input).equals(true))
  }

  it should "handle list of passwords" in {
    val input = "1-3 a: aaa\n1-3 a: aaaa\n2-3 b: bsbc"
//    Source.fromResource("inputDay2part1.csv").getLines.toList.map(_.toInt)
    assert(Day2.checkPasswordsCount(input.split("\\n").toList).equals(2))
  }

  it should "check the ositions" in {
    val input = "1-3 a: aab"
    assert(Day2.checkPositions(input).equals(true))
  }

  it should "do the thing" in {

    val input = Source.fromResource("inputDay2part1.csv").getLines.toList
//    print(Day2.checkPasswordsCount(input))
    print(Day2.checkPasswordsPositions(input))

  }
}
