import scala.annotation.tailrec

object Day7 {

  def findRequiredNumber(input: Map[String, Map[String, Int]], contentColors: String): Int = {
    val relevant = input.filter(x => contentColors.equals(x._1))
    relevant.map(col => findRequiredNumber(input, relevant(col._1))).toList.sum
  }

  def findRequiredNumber(input: Map[String, Map[String, Int]], contentColors: Map[String, Int]): Int = {
    val relevant = input.filter(x => contentColors.contains(x._1))

    if(relevant.isEmpty){
      contentColors.values.toList.sum
    } else {
      contentColors.map(col => col._2 + col._2 * findRequiredNumber(input, relevant(col._1))).toList.sum
    }
  }

  def findOutmostColors(input: Map[String, Map[String, Int]], contentColors: Set[String]): Set[String] = {
    val set = input.filter(_._2.keys.toSet.intersect(contentColors).nonEmpty).keys.toSet

    if (set.isEmpty) {
      set
    } else {
      set ++ findOutmostColors(input, set)
    }
  }

  def parseRule(input: String): (String, Map[String, Int]) = {
    if (input.contains("no other")) {
      (input.split(" ").take(2).mkString(" "), Map.empty)
    } else {
      val split = input.split("contain").toList

      val color = split.head.split(" ").take(2).mkString(" ")

      val content: Map[String, Int] = split.last.split(",").toList.map(_.trim)
        .map(_.split(" ").take(3))
        .map(x => (x.tail.mkString(" "), x.head.toInt)).toMap

      (color, content)
    }
  }
}
