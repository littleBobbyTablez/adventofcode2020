import scala.math.ceil

object Day5 {
  def parsePass(input: String): (Int, Int) = {
    val list = input.split("").toList
    val rowList = list.take(7)
    val seatList = list.takeRight(3)

    val row = rowList.foldLeft((0, 127)){  (acc, i) =>
      val rowTupel: (Int, Int) = matchRow(acc, i)
      rowTupel
    }
    val seat = seatList.foldLeft((0, 7)){  (acc, i) =>
      val seatTuple: (Int, Int) = matchRow(acc, i)
      seatTuple
    }
    (row._2, seat._2)
  }

  private def matchRow(acc: (Int, Int), i: String) = {
    val foo = i match {
      case "F" | "L" => front(acc)
      case "B" | "R" => back(acc)
    }
    foo
  }

  def front(tuple: (Int, Int)): (Int, Int) = {
    val diff = tuple._2 - tuple._1
    val sub = (diff/2) + (diff%2)
    (tuple._1, tuple._2 - sub)
  }

  def back(tuple: (Int, Int)): (Int, Int) = {
    val diff = tuple._2 - tuple._1
    val add = (diff/2)
    (tuple._1 + add, tuple._2)
  }
}
