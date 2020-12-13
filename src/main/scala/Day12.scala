object Day12 {


  case class Position(north: Int, east: Int, facing: Char)
  case class PositionWithWayPoint(north: Int, east: Int, facing: Char, wayPoint: WayPoint)
  case class WayPoint(north: Int, east: Int)
  val rotationMap = Map(0 -> 'N', 1 -> 'E', 2 -> 'S', 3 -> 'W')
  val directionMap = rotationMap.map(_.swap)

  def findPositionByWaypoint(input: List[String]): PositionWithWayPoint = {
    val instructions = input.map(parseInstruction)
    instructions.foldLeft(PositionWithWayPoint(0, 0, 'E', WayPoint(1, 10))) { (acc, i) =>
      moveToWayPoint(acc, i)
    }

  }

  def rotateWayPoint(point: WayPoint, direction: Char, degrees: Int): WayPoint = {
    val degreesRight = direction match {
      case 'R' => degrees%360
      case 'L' => 360 - (degrees%360)
    }

    degreesRight match {
      case 90 => WayPoint(0 - point.east, point.north)
      case 180 => WayPoint(0 - point.north, 0 - point.east)
      case 270 => WayPoint(point.east, 0 - point.north)
      case 0 => point
    }
  }

  def parseInstruction(input: String): (Char, Int) = {
    (input.head, input.tail.toInt)
  }

  def findPosition(input: List[String]): Position = {
    val instructions = input.map(parseInstruction)
    instructions.foldLeft(Position(0, 0, 'E')) { (acc, i) =>
      move(acc, i)
    }
  }

  def rotate(direction: Char, facing: Char, degrees: Int): Char = {
    val degreesRight = direction match {
      case 'R' => degrees
      case 'L' => 360 - (degrees%360)
    }

    rotationMap(((degreesRight/90) + directionMap(facing))%4)
  }

  private def move(acc: Position, i: (Char, Int)) : Position = {
    i._1 match {
      case 'N' => Position(acc.north + i._2, acc.east, acc.facing)
      case 'E' => Position(acc.north, acc.east + i._2, acc.facing)
      case 'S' => Position(acc.north - i._2, acc.east, acc.facing)
      case 'W' => Position(acc.north, acc.east - i._2, acc.facing)
      case 'R'|'L' => Position(acc.north, acc.east, rotate(i._1, acc.facing, i._2))
      case 'F' => move(acc, (acc.facing, i._2))
    }
  }

  private def moveToWayPoint(acc: PositionWithWayPoint, i: (Char, Int)) : PositionWithWayPoint = {
    i._1 match {
      case 'N' => PositionWithWayPoint(acc.north , acc.east, acc.facing, WayPoint(acc.wayPoint.north + i._2, acc.wayPoint.east))
      case 'E' => PositionWithWayPoint(acc.north , acc.east, acc.facing, WayPoint(acc.wayPoint.north , acc.wayPoint.east + i._2))
      case 'S' => PositionWithWayPoint(acc.north , acc.east, acc.facing, WayPoint(acc.wayPoint.north - i._2, acc.wayPoint.east))
      case 'W' => PositionWithWayPoint(acc.north , acc.east, acc.facing, WayPoint(acc.wayPoint.north, acc.wayPoint.east - i._2 ))
      case 'R'|'L' => PositionWithWayPoint(acc.north, acc.east, acc.facing, rotateWayPoint(acc.wayPoint, i._1, i._2))
      case 'F' => PositionWithWayPoint(acc.north + (i._2 * acc.wayPoint.north), acc.east + (i._2 * acc.wayPoint.east), acc.facing, acc.wayPoint)
    }
  }
}
