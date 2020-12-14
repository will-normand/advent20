package advent20

import advent20.utils.InputFileReader

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends App {
  val lines = new InputFileReader("input14").getLines

  val maskRegex = "mask = ([X01]+)".r
  val instructionRegex = "mem\\[([0-9]+)] = ([0-9]+)".r

  val groups = parseLines(lines)
  val memory = runInstructions(groups)
  val answer = memory.values.sum
  println("Answer (part 1) " + answer)

  def runInstructions(groups: List[InstructionGroup]): Map[Long, Long] = {
    val memory = mutable.Map[Long, Long]()
    groups.foreach(group =>
      group.instructions.foreach(instruction => {
        val valueAfterOnesMask = group.onesMask | instruction.value
        val valueAfterZerosMask = group.zerosMask & valueAfterOnesMask
        memory.put(instruction.index, valueAfterZerosMask)
      }))
    memory.toMap
  }

  def parseLines(lines: List[String]): List[InstructionGroup] = {
    @tailrec
    def parseLines0(lines: List[String], currentGroup: List[Instruction], groups: List[InstructionGroup]):
    List[InstructionGroup] = {
      if (lines.isEmpty) groups
      else
        lines.head match {
          case maskRegex(mask) => parseLines0(lines.tail, List.empty, InstructionGroup(mask, currentGroup) :: groups)
          case instructionRegex(index, value) =>
            parseLines0(lines.tail, Instruction(index.toInt, value.toInt) :: currentGroup, groups)
        }
    }

    parseLines0(lines.reverse, List.empty, List.empty)
  }

  def parseGroup(lines: List[String]) = {
    val maskString = lines.head match {
      case maskRegex(mask) => mask
    }
    val instructions = for (line <- lines.tail) yield {
      line match {
        case instructionRegex(index, value) => Instruction(index.toInt, value.toInt)
      }
    }

    InstructionGroup(maskString, instructions)
  }
}

case class InstructionGroup(mask: String, instructions: List[Instruction]) {
  val onesMask: Long = java.lang.Long.parseLong(mask.replace('X', '0'), 2)
  val zerosMask: Long = java.lang.Long.parseLong(mask.replace('X', '1'), 2)
}

case class Instruction(index: Int, value: Int)
