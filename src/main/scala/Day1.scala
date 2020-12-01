object day1 {

  def getMultipleOfTwo(list: List[Int]): Int = {
    list.combinations(2).toList
      .filter(x => (x.head + x.tail.head).equals(2020))
      .map(y => y.head * y.tail.head)
      .head
  }

  def getMultipleOfThree(list: List[Int]): Int = {
    list.combinations(3).toList
      .filter(x => (x.head + x.tail.head + x.tail.tail.head).equals(2020))
      .map(y => y.head * y.tail.head * y.tail.tail.head)
      .head
  }

}