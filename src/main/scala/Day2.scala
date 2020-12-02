object Day2 {
  def checkPositions(input: String): Boolean = {
    val data = parseInput(input)

    val pw = data(3)
    val pos1 = data(0).toInt - 1
    val pos2 = data(1).toInt - 1
    val letter = data(2).head

    pw(pos1).equals(letter) ^ pw(pos2).equals(letter)

  }

  def checkOnePw(input: String): Boolean = {
    val data = parseInput(input)

    val count = countLetters(data(2).head, data(3))
    data(0).toInt <= count && count <= data(1).toInt

  }

  private def parseInput(input: String) = {
     input.split("\\s").flatMap(_.split("-")).toList
  }

  def countLetters(letter: Char, input: String):Int = {
    input.toList.count(letter.equals(_))
  }

  def checkPasswordsCount(rules: List[String]): Int = {
    rules.map(checkOnePw).count(_.equals(true))
  }
  def checkPasswordsPositions(rules: List[String]): Int = {
    rules.map(checkPositions).count(_.equals(true))
  }
}
