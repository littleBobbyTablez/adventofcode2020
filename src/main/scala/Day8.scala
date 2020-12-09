object Day8 {
  def findCorruption(input: List[List[String]], acc: Int): List[(Int, String)] = {
   val fixedInput: List[(List[String], Int)] = (input:::List(List("end", "+0"))).zipWithIndex

   fixedInput.map(x => {
     val updated = fixedInput.patch(x._2, List(fixInstruction(x)), 1).map(_._1)
     run(updated, 0)
        })
  }


  private def fixInstruction(x: (List[String], Int)): (List[String], Int) = {
    (List(fixInstruction(x._1.head), x._1.last), x._2)
  }

  def fixInstruction(ins : String): String = {
    ins match {
      case "nop" => "jmp"
      case "jmp" => "nop"
      case "acc" => "acc"
      case "end" => "end"
    }
  }

  def run(input: List[List[String]], acc: Int, visited: List[Int] = List(0)): (Int, String)= {

    val instruction = input(visited.last)
    val output = executeInstruction(input, acc, visited, instruction)

    if (instruction.head.equals("end")) {
      println("Terminated :) with: " + acc)
      (acc, "end")
    } else if (visited.count(_.equals(output._3.last)) >= 1) {
      (acc, instruction.head)
    } else {
      run(output._1, output._2, output._3)
    }
  }

  private def executeInstruction(input: List[List[String]], acc: Int, visited: List[Int], instruction: List[String]) = {
    instruction.head match {
      case "nop" => (input, acc, visited ::: List(visited.last + 1))
      case "acc" => (input, acc + instruction.last.toInt, visited ::: List(visited.last + 1))
      case "jmp" => (input, acc, visited ::: List(visited.last + instruction.last.toInt))
      case "end" => (input, acc, visited)
    }
  }
}
