import org.scalatest.FlatSpec

import scala.io.Source

class Day13Test extends FlatSpec {


  it should "poc" in {
    //    val input = Source.fromResource("exampleInputDay13.csv").getLines().toList
    val input = Source.fromResource("inputDay13part1.csv").getLines().toList

    val time: Int = input.head.toInt
    val busses: List[Int] = input.last.split(",").toList.filterNot(_.equals("x")).map(_.toInt)
    val bla: (Int, Int) = busses
      .map((time / _)).map(_ + 1).zip(busses).map(x => (x._1 * x._2 - time, x._2)).minBy(_._1)
    println(bla)
    println(bla._1 * bla._2)
  }

  it should "measure earliest timestamp" in {
//    val input = Source.fromResource("exampleInputDay13.csv").getLines().toList
    val input = Source.fromResource("inputDay13part1.csv").getLines().toList
    val busses: List[String] = input.last.split(",").toList

    print(Day13.start(busses))

  }

  it should "combine busses" in {
    val bus1 = (7.toLong, 0.toLong, 0.toLong)
    val bus2 = (13.toLong, 1.toLong, 0.toLong)

    val uberbus = Day13.combineBusses(bus1, bus2)

    assert(uberbus.equals(91.toLong, 0.toLong, 77.toLong))

  }
}