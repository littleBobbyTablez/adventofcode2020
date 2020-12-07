import Day6.{countDistictLetters, countLettersIneveryAnswer}
import org.scalatest.FlatSpec

import scala.io.{BufferedSource, Source}

class Day6Test extends FlatSpec {

  it should "cout distict letters" in {
    val input = "abc\ndef"

    assert(countDistictLetters(input).equals(6))
  }

  it should "count letters in every form" in {
        val input = "abc\nadef"


//    println(List("a", "b", "c").union(List("a", "d", "e", "f")))
    assert(Day6.countLettersIneveryAnswer(input).equals(1))

  }

  it should "sum all counts" in {
    val input = Source.fromResource("inputDay6part1.csv").mkString.split("\\n\\n").toList

    println(input.map(countDistictLetters).sum)
    println(input.map(countLettersIneveryAnswer).sum)

  }
}
