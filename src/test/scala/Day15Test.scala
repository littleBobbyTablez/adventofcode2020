import org.scalatest.FlatSpec

class Day15Test extends FlatSpec {

  it should "play the game" in {

//    val input = Map(0 -> 1, 3 -> 2, 6 -> 3)
    val input = Map(0 -> 1, 20 -> 2, 7 -> 3, 16 -> 4, 1 -> 5, 18 -> 6, 15 -> 7)

    val turns = 8 to 30000000


    val map = turns.foldLeft((input, 0)){ (acc, turn) =>
      val last: Int = acc._1.getOrElse(acc._2, -1)
      if (turn.equals(30000000)) println(acc._2)
      step(acc, turn, last)
    }


  }


  private def step(acc: (Map[Int, Int], Int), turn: Int, last: Int) = {
    if (last.equals(-1)) {
      (acc._1.updated(acc._2, turn), 0)
    }
    else {
      (acc._1.updated(acc._2, turn), turn - last)
    }
  }
}
