import scala.collection.JavaConverters._

object Day11 {
  def checkRecWithSight(input: List[List[String]]): List[List[String]] = {
    val checked = checkAllWithSight(input)

    val bool = checked.equals(input)

    if (bool) {

//      println("-------------------------------------------")
//      checked.foreach(x => println(x.mkString))

      checked
    } else {
//      println("-------------------------------------------")
//      checked.foreach(x => println(x.mkString))
      checkRecWithSight(checked)
    }
  }

  def checkAllWithSight(input: List[List[String]]): List[List[String]] = {
    val transformed = transformInput(input)

    val indexed = input.map(x => x.zipWithIndex).zipWithIndex

    indexed.map(x => x._1.map(y => checkSeatWithSight(x._2 + 1, y._2 + 1, transformed)))
  }

  def checkSeatWithSight(x: Int, y: Int, input: List[List[String]]): String = {
    val seat = input(x)(y)
    val zipped = input.map(_.zipWithIndex).zipWithIndex
    val seatsInSight = countSeatsInSight(getLines(zipped, x, y))

    if (seat.equals("#") && seatsInSight >= 5) {
      "L"
    } else if (seat.equals("L") && seatsInSight.equals(0)) {
      "#"
    } else {
      seat
    }
  }

  def countSeatsInSight(input: Lines): Int = {
    val h = ocupiedInSight(input.horizontal.updated(input.y, "o"))
    val v = ocupiedInSight(input.vertical.updated(input.x, "o"))
    val dl = ocupiedInSight(input.diagonalLeft.updated(input.x, "o"))
    val dr = ocupiedInSight(input.diagonalRight.updated(input.x, "o"))

    h + v + dl + dr
  }

  case class Lines(horizontal: List[String], vertical: List[String], diagonalLeft: List[String], diagonalRight: List[String], x: Int, y: Int)

  def getLines(input: List[(List[(String, Int)], Int)], x: Int, y: Int): Lines = {
    val horizontal: List[String] = input(x)._1.map(_._1)
    val vertical: List[String] = input.map(xi => xi._1.filter(_._2.equals(y)).head).map(_._1)
    val diagonalLeft: List[String] = input.map(xi => xi._1.filter(yi => (xi._2 + yi._2).equals(x + y))).map(z => if (z.isEmpty) List((".", 0)) else z).map(_.head._1)
    val diagonalRight: List[String] = input.map(xi => xi._1.filter(yi => (xi._2 - yi._2).equals(x - y))).map(z => if (z.isEmpty) List((".", 0)) else z).map(_.head._1)

    Lines(horizontal, vertical, diagonalLeft, diagonalRight, x, y)

  }

  def ocupiedInSight(input: List[String]): Int = {
    val front = input.slice(0, input.indexOf("o"))
    val back = input.slice(input.indexOf("o"), input.length)

    if (findFirst(front.reverse) && findFirst(back)) 2
    else if (findFirst(front.reverse) || findFirst(back)) 1
    else 0

  }

  def findFirst(list: List[String]): Boolean = {
    if (list.isEmpty) false
    else {
      val seat = list.head
      if (seat.equals("L")) false
      else if (seat.equals("#")) true
      else findFirst(list.tail)
    }

  }


  def checkRec(input: List[List[String]]): List[List[String]] = {
    val checked = checkAll(input)

    val bool = checked.equals(input)

    if (bool) checked else checkRec(checked)
  }

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for {x <- xs; y <- ys} yield (x, y)
  }

  def checkAll(input: List[List[String]]): List[List[String]] = {
    val transformed = transformInput(input)

    val indexed = input.map(x => x.zipWithIndex).zipWithIndex

    indexed.map(x => x._1.map(y => checkSeat(x._2 + 1, y._2 + 1, transformed)))
  }

  def checkSeat(x: Int, y: Int, input: List[List[String]]): String = {
    val seat = input(x)(y)
    val lines = input(x).slice(y - 1, y + 2)
    val grid = input(x - 1).slice(y - 1, y + 2) ::: List(lines.head, lines.last) ::: input(x + 1).slice(y - 1, y + 2)

    if (seat.equals("#") && grid.count(_.equals("#")) >= 4) {
      "L"
    } else if (seat.equals("L") && !grid.contains("#")) {
      "#"
    } else {
      seat
    }
  }

  def transformInput(input: List[List[String]]): List[List[String]] = {
    val paddedSites: List[List[String]] = input.map(List(".") ::: _ ::: List("."))
    val frontAndBack: List[List[String]] = List(List.fill(paddedSites.head.size)("."))

    frontAndBack ::: paddedSites ::: frontAndBack
  }


}
