object Day13 {

  def isTrue(bAndM: List[(Long, Long, Long)], time: Long) = {
    bAndM.map(x => ((x._2 + time - x._3) % x._1).equals(0.toLong)).forall(i => i)
  }

  def start(busses: List[String]) = {
    val bAndM: List[(Long, Long, Long)] = busses.zipWithIndex.filterNot(_._1.equals("x")).map(x => (x._1.toLong, x._2.toLong, 0.toLong))

    val result: (Long, Long, Long) = bAndM.tail.foldLeft(bAndM.head) { (acc, i) => combineBusses(acc, i)}
    result
  }

  def findTimestamp(bAndM: List[(Long, Long, Long)] , time: Long, step: Long): (Long, Long) = {
    if (isTrue(bAndM, time)) (time, step) else findTimestamp(bAndM, time + step, step)
  }

  def combineBusses(bus1: (Long, Long, Long), bus2: (Long, Long, Long)): (Long, Long, Long) = {
    val bigger = List(bus1, bus2).maxBy(_._1)
    val first = findTimestamp(List(bus1, bus2), bigger._3 - bigger._2, bigger._1)
    val second = findTimestamp(List(bus1, bus2), first._1 + bigger._1 , bigger._1)

    (second._1 - first._1, 0.toLong, first._1)
  }



}
