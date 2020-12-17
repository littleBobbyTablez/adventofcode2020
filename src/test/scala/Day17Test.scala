import org.scalatest.FlatSpec

import scala.io.Source

class Day17Test extends FlatSpec {
  it should "find neighbours" in {

    val cube = Cube(1, 1, 1)

    assert(Day17.findNeighbours(cube).size.equals(26))

  }

  it should "check state" in {

    val active = List(Cube(1, 1, 1), Cube(3,1,1), Cube(2, 0, 0))

    assert(Day17.step(active).equals(List(Cube(2,0,0), Cube(2,0,1), Cube(2,1,0), Cube(2,1,1))))

  }



  it should "do the thing" in {

    val input = Source.fromResource("inputDay17part1.csv").getLines().toList.zipWithIndex

    val start: List[Cube] = input.flatMap(x => {
      val indexed: List[(String, Int)] = x._1.split("").toList.zipWithIndex
      indexed.map(c => c._1 match {
        case "#" => Some(Cube(x._2, c._2, 0))
        case _ => None
      }).filter(_.isDefined).map(_.get)
    })


    println((0 to 5).foldLeft(start){ (acc, i) =>
      Day17.step(acc)
    }.size)

    val start4 = start.map(x => Cube4(x.x, x.y, x.z, 0))

    println((0 to 5).foldLeft(start4){ (acc, i) =>
      Day17.step4(acc)
    }.size)
  }

}
