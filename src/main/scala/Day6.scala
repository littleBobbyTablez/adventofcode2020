object Day6 {
  def countLettersIneveryAnswer(input: String): Int = {
   val list = input.split("\\n").toList.map(_.split("").toList)

    list.tail.foldLeft(list.head) { (acc, i) => acc.intersect(i) }.size
  }

  def countDistictLetters(input: String): Int = {
    input.split("\\n").flatMap(_.split("")).toList.distinct.size
  }

}
