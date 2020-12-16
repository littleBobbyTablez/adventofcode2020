import Day16.{invalidFields, mapRulesToColumns}
import org.scalatest.FlatSpec

import scala.io.Source

class Day16Test extends FlatSpec {

  val rulesRaw = Source.fromResource("inputDay16part1.csv").getLines().toList
  val rules = Day16.parseRules(rulesRaw)

  it should "parse rules" in {

    println(Day16.parseRules(rulesRaw))

  }

  it should "validate Tickets" in {


    val tickets: List[List[Int]] = Source.fromResource("inputDaz16part2.csv").getLines().toList.map(_.split(",").toList.map(_.toInt))

    println(tickets.flatMap(invalidFields(_, rules)).sum)
  }

  it should "validate one Ticket" in {
    val ticket = List(40,4,50)
    val rules = List((1, 3), (5, 7), (6, 11), (33, 44), (13, 40), (45, 50))


    println(invalidFields(ticket, rules))//.equals(List(4)))


  }

  it should "check number" in {
    val number = 40
    val rules = List((1, 3), (5, 7), (6, 11), (33, 44), (13, 40), (45, 50))

    assert(Day16.checkNumber(rules, number))
  }

  it should "find Rows" in {
    val rulesRaw = Source.fromResource("inputDay16part1.csv").getLines().toList
    val tickets: List[List[Int]] = Source.fromResource("inputDaz16part2.csv").getLines().toList.map(_.split(",").toList.map(_.toInt))

    val mapping: List[(Int, Int)] = mapRulesToColumns(tickets, rulesRaw).take(6)

    val myticket = List(109,101,79,127,71,59,67,61,173,157,163,103,83,97,73,167,53,107,89,131)

    println(mapping.map(x => myticket(x._2)).map(BigInt(_)).product)
    println(mapping)

  }



}
