import Day4.{checkPassports, transformInput}
import org.scalatest.FlatSpec

import scala.io.{BufferedSource, Source}

class Day4Test extends FlatSpec {
  it should "check validity" in {

    val input = List("ecl:gry", "pid:860033327", "eyr:2020", "hcl:#fffffd", "byr:1937", "iyr:2017", "cid:147", "hgt:183cm")

    assert(Day4.checkValidity(input).equals(true))

  }

  it should "transform input" in {

    val input ="ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

    assert(transformInput(input)(0).size.equals(8))
  }


  it should "do the thing" in {

    val input = Source.fromResource("inputDay4part1.csv").mkString

    print(checkPassports(transformInput(input)))

  }

}
