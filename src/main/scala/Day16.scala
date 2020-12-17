object Day16 {
  def invalidFields(ticket: List[Int], rules: List[(Int, Int)]): List[Int] = {

    ticket.filter(y =>
      checkNumber(rules, y))

  }

  def checkNumber(rules: List[(Int, Int)], y: Int): Boolean = {
    !rules.exists(z => (z._1 to z._2 contains y))
  }

  def parseRules(rules: List[String]): List[(Int, Int)] = {
    rules.flatMap(x => x.split(": ")
      .tail.head.split(" or ").toList
      .map(_.split("-"))
      .map(y => (y.head.toInt, y.last.toInt)))
  }



  def mapRulesToColumns(tickets: List[List[Int]], rulesRaw: List[String]): List[(Int, Int)] = {
    val rules = parseRules(rulesRaw)

    val filtered = tickets.filter(invalidFields(_, rules).isEmpty).flatMap(_.zipWithIndex)
    val column = filtered.groupBy(_._2).mapValues(_.map(_._1))

    val groupedAndIndexed = rules.grouped(2).toList.zipWithIndex

    val rulesToRows = groupedAndIndexed.map(rule => (rule._2, column.filter(row => invalidFields(row._2, rule._1).isEmpty).keys))

    rulesToRows.toList.sortBy(_._2.size).foldLeft(List[(Int, Int)]()) { (acc, i) =>
      acc ::: List((i._1, i._2.filterNot(acc.map(_._2) contains _).head))
    }.sortBy(_._1)
  }

}
