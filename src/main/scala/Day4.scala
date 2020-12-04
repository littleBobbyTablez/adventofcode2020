object Day4 {
  def transformInput(input: String): List[List[String]] = {
    input.split("\\n\\n").map(_.split("\\s").toList).toList
  }

  val patternsMap = Map("byr" -> "19[2-9][0-9]|200[0-2]", "iyr" -> "201[0-9]|2020", "eyr" -> "202[0-9]|2030",
    "hgt" -> "1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in", "hcl" -> "#[0-9a-f]{6}",
    "ecl" -> "amb|blu|brn|gry|grn|hzl|oth", "pid" -> "[0-9]{9}", "cid" ->".*")

  val notOptionalFileds = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

  def checkValidity(passport: List[String]): Boolean = {
    val real = passport.map(_.split(":")).map(_.head).toSet
    notOptionalFileds.subsetOf(real)
  }

  def checkValidityOfFields(passport: List[String]): Boolean = {
    if (checkValidity(passport)) {
      passport.map(_.split(":")).forall(x => x(1).matches(patternsMap(x(0))))
    } else {
      false
    }
  }

  def checkPassportsAndFields(list: List[List[String]]): Int = {
    list.map(checkValidityOfFields).count(_.equals(true))
  }

  def checkPassports(list: List[List[String]]): Int = {
    list.map(checkValidity).count(_.equals(true))
  }

}
