import org.scalatest.FlatSpec

import scala.io.Source

class Day9Test extends FlatSpec {

  it should "check validity" in {
    val preamp = List(1,2,3,4,5).map(BigInt(_))
    val number = BigInt(7)

    assert(Day9.checkValidity(number, preamp))
  }

  it should "check List" in {
    val input = Source.fromResource("exampleInputDay9.csv").getLines().map(BigInt(_)).toList

    assert(Day9.findFirstNotValid(input, 5).equals(BigInt(127)))

  }

  it should "do the thing" in {
    val input = Source.fromResource("inputDay9part1.csv").getLines().map(BigInt(_)).toList

    println(Day9.findFirstNotValid(input, 25))

    println(Day9.findSetRec(input, 2089807806,3))
  }

  it should "find set" in {
    val input = Source.fromResource("exampleInputDay9.csv").getLines().map(BigInt(_)).toList

    assert(Day9.findSetRec(input, 127,3).equals(BigInt(62)))
  }

}
