package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec

object Day08 extends App {
  val lines = new InputFileReader("input08").getLines.toVector

  val instructions = lines map OpCode.fromString
  instructions map println

  val _, result1 = new Interpreter(instructions).run
  println("Answer (part 1) " + result1)

  val result2 = tryPrograms(instructions)
  println("Answer (part 2) " + result2)

  def tryPrograms(instructions: Vector[OpCode]): Int = {
    val possiblePrograms = generatePossiblePrograms(instructions)
    val result = possiblePrograms map (new Interpreter(_).run) find (_._1)
    result.get._2
  }

  private def generatePossiblePrograms(instructions: Vector[OpCode]): Seq[Array[OpCode]] = {
    def switchOps(opCode: OpCode): OpCode = opCode match {
      case Jmp(arg) => Nop(arg)
      case Nop(arg) => Jmp(arg)
      case Acc(arg) => Acc(arg)
    }

    val possiblePrograms = instructions.zipWithIndex.map {
      case (instruction, index) => {
        val newInstructions = instructions.toArray
        newInstructions(index) = switchOps(instruction)
        newInstructions
      }
    }
    possiblePrograms
  }
}

class Interpreter(instructions: IndexedSeq[OpCode]) {
  def run: (Boolean, Int) = {
    op(0, 0, Set.empty)
  }

  @tailrec
  private def op(p: Int, acc: Int, history: Set[Int]): (Boolean, Int) = {
    if (history.contains(p)) (false, acc)
    else if (p == instructions.length) (true, acc)
    else instructions(p) match {
      case Acc(arg) => op(p + 1, acc + arg, history + p)
      case Jmp(arg) => op(p + arg, acc, history + p)
      case Nop(_) => op(p + 1, acc, history + p)
    }
  }
}

sealed trait OpCode

object OpCode {
  private val instructionRegex = "([a-z]{3}) ([+-][0-9]+)".r

  def fromString(instruction: String): OpCode = instruction match {
    case instructionRegex(op, arg) =>
      op match {
        case "acc" => Acc(arg.toInt)
        case "jmp" => Jmp(arg.toInt)
        case "nop" => Nop(arg.toInt)
      }
  }
}

case class Acc(arg: Int) extends OpCode

case class Jmp(arg: Int) extends OpCode

case class Nop(arg: Int) extends OpCode
