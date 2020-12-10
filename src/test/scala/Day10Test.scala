import org.scalatest.FlatSpec

import scala.io.Source
class Day10Test extends FlatSpec {
  it should "find diffs" in {
//    val input = List(16,10,15,5,1,11,7,19,6,12,4)

    val input = Source.fromResource("exampleInputDay10.csv").getLines().toList.map(_.toInt)

    println(Day10.findDiff(input))
    assert(Day10.findDiff(input).equals(220))

  }

  it should "find diffs for real " in {
    val input = Source.fromResource("inputDay10part1.csv").getLines().toList.map(_.toInt)

    println(Day10.findDiff(input))

  }


  it should "find possible arrangements " in {
//    val input = List(16,10,15,5,1,11,7,19,6,12,4)
    val input = Source.fromResource("exampleInputDay10.csv").getLines().toList.map(_.toInt)

    print(Day10.findNumberOfPaths(input))
    assert(Day10.findNumberOfPaths(input).equals(19208))
  }

  it should "find possible arrangements for real " in {
    //    val input = List(16,10,15,5,1,11,7,19,6,12,4)
    val input = Source.fromResource("inputDay10part1.csv").getLines().toList.map(_.toInt)

    println(Day10.findNumberOfPaths(input))
  }


}
