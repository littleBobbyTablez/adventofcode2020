import org.scalatest.FlatSpec

import scala.io.Source

class Day8Test extends FlatSpec {
  it should "run Programm" in {
    val input = Source.fromResource("exampleInputDay8.csv").getLines().toList
    val split = input.map(_.split(" ").toList)
    assert(Day8.run(split, 0)._1.equals(5))

  }

  it should "run real Programm" in {
    val input = Source.fromResource("inputDay8part1.csv").getLines().toList
    val split = input.map(_.split(" ").toList)
    print(Day8.run(split, 0))

  }

  it should "find corruption" in {
    val input = Source.fromResource("exampleInputDay8.csv").getLines().toList
    val split = input.map(_.split(" ").toList)
    assert(Day8.findCorruption(split, 0).filter(_._2.equals("end")).head._1.equals(8))
  }

  it should "find corruption for real" in {
    val input = Source.fromResource("inputDay8part1.csv").getLines().toList
    val split = input.map(_.split(" ").toList)
    println(Day8.findCorruption(split, 0).filter(_._2.equals("end")).head)
  }
}
