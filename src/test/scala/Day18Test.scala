import java.util.regex.{Matcher, Pattern}

import Day18.step
import org.scalatest.FlatSpec

import scala.io.Source

class Day18Test extends FlatSpec {
  it should "split right" in {

//    val input = "(1 + 4 * 3) + ((2 + 4) * 3)"
    val input = "(6 + 6 * 9) * 7 * 9 + (7 * (7 + 3 * 4 + 8 * 9 + 2) + 3 * 2 * 4 + 5) + 8 * 2"
//    val input = "1 + 2 * 3 + 4 * 5 + 6"

    println(step(input))
  }


  it should "replace" in {

    val input = "(1 + 4 * 3) + ((2 + 4) * 3)"

    println(input.replace("(1 + 4 * 3)", "9"))
  }


  it should "calculate inner expresssion" in {

    val string = "(1 + 2 * 3 + 4 * 5 + 6)"

    println(Day18.calculate(string))
  }

  it should "calculate sum" in {

    val input = Source.fromResource("inputDay18part1.csv").getLines().toList


    println(input.map(step).map(BigInt(_)).sum)

  }




}
