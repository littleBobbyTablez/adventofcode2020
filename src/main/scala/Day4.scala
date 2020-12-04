object Day4 {
  def transformInput(input: String): List[List[String]] = {
    input.split("\\n\\n").map(_.split("\\s").toList).toList
  }


  val notOptionalFileds = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

  def checkValidity(passport: List[String]): Boolean = {
    val real = passport.map(_.split(":")).map(_.head).toSet
    notOptionalFileds.subsetOf(real)
  }

  def checkPassports(list: List[List[String]]): Int = {
    list.map(checkValidity).count(_.equals(true))
  }

}
