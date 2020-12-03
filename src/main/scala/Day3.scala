import scala.math._

object Day3 {
  def transformInput(input: List[List[String]], stepSize: Int) = {
    val lengthGoal = input.size * stepSize
    val rowLength = input(1).size
    val multiplier = ceil(lengthGoal/rowLength).toInt + 1

    input.map(List.fill(multiplier)(_).flatten)

  }

  def countTrees(input: List[List[String]], stepSizeRight: Int, stepSizeDown: Int) = {
   val transformed = input.zipWithIndex
     .filter(x => (x._2 % stepSizeDown).equals(0))
     .map(_._1).zipWithIndex
     .map(x => x._1(x._2*stepSizeRight))

    transformed.count(_.equals("#"))
  }


}
