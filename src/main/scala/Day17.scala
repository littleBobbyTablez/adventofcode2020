import Day11.Crossable

import scala.collection.immutable

object Day17 {
  def step(active: List[Cube]) = {

    val candidates = active.flatMap(findNeighbours).distinct

    candidates.foldLeft(List[Cube]()){ (acc, i) =>
      findNeighbours(i).intersect(active).size match {
        case 3 => acc:::List(i)
        case 2 => if (active contains i) acc:::List(i) else acc
        case _ => acc
      }
    }

  }

  def step4(active: List[Cube4]) = {

    val candidates = active.flatMap(findNeighbours4).distinct

    candidates.foldLeft(List[Cube4]()){ (acc, i) =>
      findNeighbours4(i).intersect(active).size match {
        case 3 => acc:::List(i)
        case 2 => if (active contains i) acc:::List(i) else acc
        case _ => acc
      }
    }

  }

  def findNeighbours(cube: Cube): List[Cube] = {

    val xs = cube.x - 1 to cube.x + 1
    val ys = cube.y - 1 to cube.y + 1
    val zs = cube.z - 1 to cube.z + 1

    val list = xs cross ys cross zs toList

    list.map(i => Cube(i._1._1, i._1._2, i._2)).filterNot(_.equals(cube))
  }


  def findNeighbours4(cube: Cube4): List[Cube4] = {

    val xs = cube.x - 1 to cube.x + 1
    val ys = cube.y - 1 to cube.y + 1
    val zs = cube.z - 1 to cube.z + 1
    val ws = cube.w - 1 to cube.w + 1

    val list = xs cross ys cross zs cross ws toList

    list.map(i => Cube4(i._1._1._1, i._1._1._2, i._1._2, i._2)).filterNot(_.equals(cube))
  }

}

case class Cube(x: Int, y: Int, z: Int)
case class Cube4(x: Int, y: Int, z: Int, w: Int)
