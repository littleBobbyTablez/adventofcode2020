import sun.security.util.Length

import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Day9 {
  def findFirstNotValid(input: List[BigInt], i: Int): BigInt = {

    input.zipWithIndex.takeRight(input.length - i)
      .filterNot(x =>
        checkValidity(x._1, input.zipWithIndex.filter(y => (x._2 - i until x._2).contains(y._2)).map(_._1)))
      .head._1
  }

  def checkValidity(number: BigInt, preamp: List[BigInt]): Boolean = {
    val candidates: List[BigInt] = preamp.flatMap(x => preamp.filterNot(x.equals).map(_ + x))

    candidates.contains(number)
  }


  def findSetRec(input: List[BigInt], sum: BigInt): BigInt = {
    findSetRec(input, sum, 3)
  }

  @tailrec
  def findSetRec(input: List[BigInt], sum: BigInt, lenght: Int): BigInt = {

    val result: List[List[BigInt]] = findSetofNumbers(input, sum, lenght)

    if (result.isEmpty) {
      findSetRec(input, sum, lenght + 1)
    } else {
      result.head.max + result.head.min
    }
  }

  def findSetofNumbers(input: List[BigInt], sum: BigInt, length: Int): List[List[BigInt]] = {
    toGroups(input, length)
      .filter(_.sum.equals(sum))
  }

  def toGroups(input: List[BigInt], length: Int): List[List[BigInt]] = {
    if (input.length.equals(length)) {
      List(input)
    } else {
      val group: List[BigInt] = input.take(length)
      List(group):::toGroups(input.tail, length)
    }
  }
}
