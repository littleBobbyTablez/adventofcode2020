import scala.math.BigInt.int2bigInt

object Day14 {

  def findAdresses(entry: List[String], mask: String): List[BigInt] = {
    List(BigInt(1))

  }


  case class State(map: Map[BigInt, String], mask: String)

  def run(input: List[List[String]]): BigInt = {
    val mask = input.head.last
    runRec(input.tail, mask)

  }
  def runRec(tail: List[List[String]], mask: String): BigInt = {
    val endState = tail.foldLeft(State(Map.empty, mask)) { (acc, i) =>
      i.head match {
        case "mask" => State(acc.map, i.last)
        case _ => State(addOrUpdate(i, acc.map, acc.mask) , acc.mask)
      }
    }
    endState.map.values.toList.map(BigInt(_, 2)).sum
  }

  def runFloating(input: List[List[String]]): BigInt = {
    val mask = input.head.last
    runRecFloating(input.tail, mask)

  }
  def runRecFloating(tail: List[List[String]], mask: String): BigInt = {
    val endState = tail.foldLeft(State(Map.empty, mask)) { (acc, i) =>
      i.head match {
        case "mask" => State(acc.map, i.last)
        case _ => State(addOrUpdateFloating(i, acc.map, acc.mask) , acc.mask)
      }
    }
    endState.map.values.toList.map(BigInt(_)).sum
  }

  def addOrUpdate(entry: List[String], map: Map[BigInt, String], mask: String): Map[BigInt, String] = {
    val bank = entry.head.substring(4, entry.head.length - 1).toInt
    map.updated(bank, applyMask(entry.last.toInt, mask))
  }

  def applyMask(update: BigInt, mask: String): String = {
    val value = update.toString(2)
    val list = List.fill(36 - value.length)(0):::value.split("").toList
    val binary = list.zip(mask.split("")).map(x => if (x._2.equals("X")) x._1 else x._2).mkString

    binary
  }

  def addOrUpdateFloating(entry: List[String], map: Map[BigInt, String], mask: String): Map[BigInt, String] = {
    val bank = BigInt(entry.head.substring(4, entry.head.length - 1))
    val banks = applyMaskFloating(bank, mask)
    banks.foldLeft(map){(acc, i) =>
      acc.updated(i, entry.last)
    }
  }

  def applyMaskFloating(update: BigInt, mask: String): List[BigInt] = {
    val value = update.toString(2)
    val list = List.fill(36 - value.length)("0"):::value.split("").toList
    val floating: List[String] = list.zip(mask.split("")).map(x =>
    x._2 match {
      case "0" => x._1
      case "1" => "1"
      case "X" => "X"
    })

    val combos: List[String] = findCombinations(floating.count(_.equals("X")))

    combos.map(replace(floating, _)).map(BigInt(_, 2))
  }

  def findCombinations(x : Int): List[String] = {
    val list = (BigInt(0) until BigInt(2).pow(x)).toList.map(_.toString(2))
    list.map(x => List.fill(list.last.length - x.length)("0").mkString + x)
  }

  def replace(floating: List[String], replace: String) = {
    floating.toList.foldLeft((List[String](), replace.split("").toList)) { (acc, i) =>
      i match {
        case "0" => (acc._1:::List("0"), acc._2)
        case "1" => (acc._1:::List("1"), acc._2)
        case "X" => (acc._1:::List(acc._2.head), acc._2.tail)
      }
    }._1.mkString
  }

  //  def update(list: List[String], update: (Int, Int)) = {
  //    val value = update._1.toBinaryString
  //    list.updated(update._2, List.fill(36 - value.length)(0).mkString+value)
  //  }

}
