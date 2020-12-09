import Day9.{findFirstNotValid, findSetRec}
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

    assert(findFirstNotValid(input, 5).equals(BigInt(127)))
  }

  it should "do the thing" in {
    val input = Source.fromResource("inputDay9part1.csv").getLines().map(BigInt(_)).toList

    val part1 = findFirstNotValid(input, 25)
    println(part1)
    println(findSetRec(input, part1))
  }

  it should "find set" in {
    val input = Source.fromResource("exampleInputDay9.csv").getLines().map(BigInt(_)).toList

    assert(findSetRec(input, 127).equals(BigInt(62)))
  }

}
