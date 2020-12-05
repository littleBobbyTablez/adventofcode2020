import Day5.parsePass
import org.scalatest.FlatSpec

import scala.io.Source

class Day5Test extends FlatSpec {
  val input = "FBFBBFFRLR"
  val real = Source.fromResource("inputDay5part1.csv").getLines.toList

  it should "parse Boarding pass" in {

    val seatIds = real.map(parsePass).map(x => (x._1 * 8) + x._2)
    println(seatIds.max)

    print((0 to 938).diff(seatIds))
//    assert(Day5.parsePass(input).equals(List(44,5)))
  }
}
