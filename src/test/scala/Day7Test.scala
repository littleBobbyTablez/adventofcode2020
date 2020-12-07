import Day7.parseRule
import org.scalatest.FlatSpec

import scala.io.Source

class Day7Test extends FlatSpec {

  it should "parse one rule" in {

    val input = "light red bags contain 1 bright white bag, 2 muted yellow bags."
    println(parseRule(input))
  }


  it should "parse empty bags" in {

    val input = "faded blue bags contain no other bags."
    println(parseRule(input))
  }


  it should "find gold bags" in {

    val input = Source.fromResource("exampleInputDay7.csv").getLines().toList.map(parseRule).toMap

    val contentColor = Set("shiny gold")
    assert(Day7.findOutmostColors(input, contentColor).equals(Set("bright white", "muted yellow", "dark orange", "light red")))

  }

  it should "find containing bags" in {
    val input = Source.fromResource("inputDay7part1.csv").getLines().toList.map(parseRule).toMap

    val i = Day7.findRequiredNumber(input, "shiny gold")
    println(i)
//    assert(i.equals(32))

  }


  it should "do the thing" in {

    val input = Source.fromResource("inputDay7part1.csv").getLines().toList.filterNot(_.contains("no other")).map(parseRule).toMap

    val contentColor = Set("shiny gold")
    print(Day7.findOutmostColors(input, contentColor).size)

  }




}
