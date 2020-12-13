import Day12.rotate
import org.scalatest.FlatSpec

import scala.io.Source

class Day12Test extends FlatSpec {
  it should "find Position" in {
    val input = Source.fromResource("inputDay12part1.csv").getLines().toList

    val position = Day12.findPosition(input)
    println(position)
    println(position.east.abs + position.north.abs)
  }



  it should "rotate correctly" in {

    assert(rotate('R', 'E', 90).equals('S'))
    assert(rotate('L', 'E', 90).equals('N'))
  }

  it should "find Position by Waypoint" in {
//    val input = Source.fromResource("exampleInptDay12.csv").getLines().toList
    val input = Source.fromResource("inputDay12part1.csv").getLines().toList

    val position = Day12.findPositionByWaypoint(input)
    println(position)
    println(position.east.abs + position.north.abs)
  }

}
