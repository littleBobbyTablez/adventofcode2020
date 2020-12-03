import Day3._
import org.scalatest.FlatSpec

import scala.io.Source
import scala.math.BigDecimal.int2bigDecimal

class Day3Test extends FlatSpec {

  val real = Source.fromResource("inputDay3part1.csv").getLines.toList
  val example = Source.fromResource("exampleInputDay3.csv").getLines.toList

  it should "count trees" in {
    val input = List(List(".",".",".","."), List(".",".","#","#"))
    assert(countTrees(input, 3, 1).equals(1))
  }

  it should "count trees every second row" in {
    val input = List(List(".",".",".","."), List(".",".","#","#"), List(".","#",".","."), List(".",".","#","#"))
    assert(countTrees(input, 1, 2).equals(1))
  }

  it should "transform input" in {
    val input = List(List(".",".",".","."), List(".",".","#","#"), List(".",".",".","."), List(".",".","#","#"))

    transformInput(input, 3)(1).size.equals(12)

  }

  it should "do the Thing" in {

    val input = transformInput(real.map(_.split("").toList), 3)

    println(countTrees(input, 3, 1))
    println()
    val input2 = transformInput(real.map(_.split("").toList), 7)

    val slope1 = countTrees(input2, 1, 1)
    val slope2 = countTrees(input2, 3, 1)
    val slope3 = countTrees(input2, 5, 1)
    val slope4 = countTrees(input2, 7, 1)
    val slope5 = countTrees(input2, 1, 2)


    println (slope1)
    println (slope2)
    println (slope3)
    println (slope4)
    println (slope5)

    val hae = List(slope1 , slope2 , slope3 , slope4 , slope5).map(_.toBigInt)
    println(hae.product)

  }




}
