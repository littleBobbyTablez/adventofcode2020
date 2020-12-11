import Day11.Lines
import org.scalatest.FlatSpec

import scala.io.Source

class Day11Test extends FlatSpec {

  it should "check seat" in {
    val input: List[List[String]] = Source.fromResource("exampleInputDay11.csv").getLines().toList.map(_.split("").toList)

    assert(Day11.checkSeat(1, 4, input).equals("#"))

  }

  it should "handle corners" in {
    val input: List[List[String]] = Source.fromResource("exampleInputDay11.csv").getLines().toList.map(_.split("").toList)
    val transformed = Day11.transformInput(input)

    transformed.foreach(x => println(x.mkString))
  }

  it should "check all seats " in {
    val input: List[List[String]] = Source.fromResource("exampleInputDay11.csv").getLines().toList.map(_.split("").toList)
    val checked = Day11.checkAll(input)

    assert(!checked.map(_.contains("L")).foldLeft(false){(acc, i) => i || acc})
  }

  it should "check recursively" in {
//    val input: List[List[String]] = Source.fromResource("exampleInputDay11.csv").getLines().toList.map(_.split("").toList)
    val input: List[List[String]] = Source.fromResource("inputday11part1.csv").getLines().toList.map(_.split("").toList)

    println(Day11.checkRecWithSight(input).map(_.count(_.equals("#"))).sum)

  }

  it should "get visible chairs in List" in {
    val input = List(".", "#", "L", "o", ".", "#", ".")

    assert(Day11.ocupiedInSight(input).equals(1))
  }

  it should "find lines " in {

    val input = List(List(".", "L", "#"),List(".", "L", "#"),List(".", "L", "#")).map(_.zipWithIndex).zipWithIndex

    assert(Day11.getLines(input, 1, 1).equals(Lines(List(".", "L", "#"),List("L", "L", "L"),List("#", "L", "."),List(".", "L", "#"), 1, 1)))

  }


  it should "count seats in sight" in {

   val input = Lines(List(".", "L", "#"),List("L", "L", "L"),List("#", "L", "."),List(".", "L", "#"), 1, 1)

    Day11.countSeatsInSight(input)

  }
}
