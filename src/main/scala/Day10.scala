
object Day10 {

  case class Path(lastEdge: (Int, Int))


  def findNumberOfPaths(input: List[Int]): Long = {
    val end = input.max
    val graph: List[List[(Int, Int)]] = findGraph(input)
    val paths: List[List[(Int, Int)]] = graph.map(findPaths(_, end))

    paths.filterNot(_.isEmpty).foldLeft(1.toLong){(acc,i) =>
      acc.toLong*i.size.toLong
    }
  }

  def findGraph(input: List[Int]): List[List[(Int, Int)]] = {
    val sorted = input.sorted
    val knodes = List(0) ::: sorted ::: List(sorted.last + 3)

    val grouped = knodes.tail.foldLeft(List(List(knodes.head))){ (acc, i) =>
      if (i - acc.last.last == 3) {
        acc:::List(List(i))
      } else {
        acc.take(acc.size - 1):::List(acc.last ::: List(i))
      }
    }

    grouped.map(_.flatMap(x => knodes.filter(y => hasEdge(x, y)).map(z => (x, z))))
  }


  def findPaths(graph: List[(Int, Int)], end: Int) = {
    val start: List[(Int, Int)] = graph.filter(_._1.equals(graph.head._1))
    val startIndex = graph.size - start.size

    graph.takeRight(startIndex).foldLeft(start) { (acc, i) =>
      val filtered: List[(Int, Int)] = acc.filter(_._2.equals(i._1))
      acc ::: filtered.map(_ => i)
    }.filter(_._2.equals(graph.last._2))
  }


  def findDiff(input: List[Int]) = {
    val sorted = input.sorted
    val withDevice = sorted ::: List(sorted.last + 3)
    val counted = withDevice.foldLeft((0, 0, 0)) { (acc, i) =>
      val diff = i - acc._3

      diff match {
        case 1 => (acc._1 + 1, acc._2, i)
        case 3 => (acc._1, acc._2 + 1, i)
        case _ => (acc._1, acc._2, i)
      }
    }
    counted._1 * counted._2

  }


  private def hasEdge(tuple: (Int, Int)): Boolean = {
    val value = tuple._2 - tuple._1
    value <= 3 && value > 0
  }

}
