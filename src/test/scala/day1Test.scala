import day1.{getMultipleOfThree, getMultipleOfTwo}
import org.scalatest.FlatSpec

import scala.io.Source

class day1Test extends FlatSpec {


  it should "add numbers" in {
    val input = List(10, 20, 30, 2000)

    val realInput = Source.fromResource("inputDay1.csv").getLines.toList.map(_.toInt)
    val realInput2 = Source.fromResource("inputDay1part2.csv").getLines.toList.map(_.toInt)
    print(getMultipleOfTwo(realInput))
    print(getMultipleOfThree(realInput2))

  }
}
