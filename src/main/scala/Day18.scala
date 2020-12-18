object Day18 {

  def step(input: String): String = {
    val p = "\\([\\s+*\\d]*\\)".r

//    println((p.findAllIn(input)).toList)

    val innerExpressions = p.findAllIn(input)
    if (innerExpressions.isEmpty) {
      calculate("(" + input + ")")
    } else {
      step(innerExpressions.foldLeft(input) { (acc, i) =>
        acc.replace(i, Day18.calculate(i))
      })
    }
  }

  def calculate(string: String) = {
    val trimmed = string.substring(1, string.length - 1)

    val plus = trimmed.split(" ").foldLeft(List[String]()) { (acc, i) =>
      if ((acc.size % 2).equals(0) && acc.nonEmpty) {
        acc.last match {
          case "+" => acc.init.init ::: List((BigInt(acc.init.last) + BigInt(i)).toString)
          case "*" => acc ::: List(i)
        }
      } else {
        acc ::: List(i)
      }
    }

    val mult = plus.foldLeft(List[String]()) { (acc, i) =>
      if (acc.size.equals(2)) {
        acc.last match {
          case "*" => List((BigInt(acc.head) * BigInt(i)).toString)
        }
      } else {
        acc ::: List(i)
      }
    }

    mult.head
  }
}
