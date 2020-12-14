import Day14.findCombinations
import org.scalatest.FlatSpec

import scala.io.Source
import scala.math.BigInt.int2bigInt

class Day14Test extends FlatSpec {
//  it should "update memory entry" in {
//    val mem = "000000000000000000000000000000000000"
//    val list = List(mem)
//    val update = (11, 0)
//
//    assert(Day14.update(list, update).head.equals("000000000000000000000000000000001011"))
//  }
//
//  it should "apply Mask" in {
//    val mem = "000000000000000000000000000000000000"
//    val list = List(mem)
//    val update = 11
//    val mask = "XXXXXXXXX1XXXXXXXXXXXXXXXXXXXXXXXXXX"
//
//    assert(Day14.update(list, (Integer.parseInt(Day14.applyMask(update, mask), 2),0)).head.equals("000000000100000000000000000000001011"))
//  }

  it should "run" in {
//    val input = Source.fromResource("exampleInputDay14pat1.csv").getLines().toList.map(_.split(" = ").toList)
    val input = Source.fromResource("inputDay14part1.csv").getLines().toList.map(_.split(" = ").toList)

//    println(Day14.run(input))
    println(Day14.runFloating(input))


  }


  it should "update map" in {
    val map = Map(BigInt(10) ->"000000000000000000000000000000000000")
    val mask = "XXXXXXXXX1XXXXXXXXXXXXXXXXXXXXXXXXXX"
    val entry = List("mem[10]", 11.toString)
    val entry2 = List("mem[11]", 11.toString)
    val map2 = Day14.addOrUpdate(entry2, map, mask)
    assert(Day14.addOrUpdate(entry, map2, mask).size.equals(2))

  }

  it should "find combinations" in {
    assert(findCombinations(8).length.equals(256))

  }


  it should "replace x " in {
    val floating = "00000000000000000000000X0000000X0000"
    val replace = "11"

    assert(Day14.replace(floating.split("").toList, replace).equals("000000000000000000000001000000010000"))
  }

  it should "update map floating" in {
    val mask = "00000000000000000000000000000000X0XX"
    val entry = ("mem[26]", "1")




  }

}
