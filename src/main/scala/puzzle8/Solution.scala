package puzzle8

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input8.txt")
    .getLines
    .toIndexedSeq

  def execute(acc: Int, idx: Int, insns: IndexedSeq[String], set: Set[Int]): Int = {
    insns(idx) match {
      case s"$ins ${int(offset)}" =>
        val (newAcc, next) = ins match {
        case "jmp" => (acc, idx + offset)
        case "acc" => (acc + offset, idx + 1)
        case "nop" => (acc, idx + 1)
        }
        if (set.contains(next)) acc
        else execute(newAcc, next, insns, set ++ Set(idx))
    }
  }

  def partOne: Int = {
    execute(0, 0, input, Set.empty[Int])
  }

  def executeProperly(acc: Int, idx: Int, insns: IndexedSeq[String], set: Set[Int]): Option[Int] = {
    if (idx >= insns.length) Some(acc)
    else
      insns(idx) match {
        case s"$ins ${int(offset)}" =>
          val (newAcc, next) = ins match {
            case "jmp" => (acc, idx + offset)
            case "acc" => (acc + offset, idx + 1)
            case "nop" => (acc, idx + 1)
          }
          if (set.contains(next)) None
          else executeProperly(newAcc, next, insns, set ++ Set(idx))
      }
  }

  def partTwo: Int = {
    val answers = input.zipWithIndex.foldLeft(Set.empty[IndexedSeq[String]]) {
      case (acc, (s"$ins $offset", idx)) if ins == "nop" => acc ++ Set(input.updated(idx, s"jmp $offset"))
      case (acc, (s"$ins $offset", idx)) if ins == "jmp" => acc ++ Set(input.updated(idx, s"nop $offset"))
      case (acc, _) => acc
    }.flatMap { modifiedInsns =>
      executeProperly(0, 0, modifiedInsns, Set.empty[Int])
    }
    answers.head
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}