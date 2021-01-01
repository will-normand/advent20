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

  val memory2 = runInstructionsV2(groups)
  val answer2 = memory2.values.sum
  println("Answer (part 2) " + answer2)

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

  def runInstructionsV2(groups: List[InstructionGroup]): Map[Long, Long] = {
    val memory = mutable.Map[Long, Long]()
    groups.foreach(group => {
      group.instructions.foreach(instruction => {
        val memoryLocations = applyMask(instruction.index, group.mask)
        memoryLocations.foreach(memory.put(_, instruction.value))
      })
    })

    memory.toMap
  }

  def applyMask(address: Long, mask: String): List[Long] = {
    def branchList(addresses: List[String]): List[String] = {
      val withZeros = addresses.map(_.concat("0"))
      val withOnes = addresses.map(_.concat("1"))
      withZeros ++ withOnes
    }

    @tailrec
    def applyMask0(address: List[Char], mask: List[Char], addresses: List[String]): List[String] = {
      mask match {
        case '0' :: bits => applyMask0(address.tail, bits, addresses.map(_.concat(address.head.toString)))
        case '1' :: bits => applyMask0(address.tail, bits, addresses.map(_.concat("1")))
        case 'X' :: bits => applyMask0(address.tail, bits, branchList(addresses))
        case _ => addresses
      }
    }

    val addressChars: List[Char] = address.toBinaryString.toList.reverse.padTo(36, '0').reverse
    applyMask0(addressChars, mask.toList, List("")) map {
      java.lang.Long.parseLong(_, 2)
    }
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
}

case class InstructionGroup(mask: String, instructions: List[Instruction]) {
  val onesMask: Long = java.lang.Long.parseLong(mask.replace('X', '0'), 2)
  val zerosMask: Long = java.lang.Long.parseLong(mask.replace('X', '1'), 2)
}

case class Instruction(index: Int, value: Int)
