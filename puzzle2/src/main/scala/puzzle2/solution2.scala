package puzzle2

import scala.io.Source

object Day01 extends App {
  def partOne: Int = {
    val input = Source.fromResource("input.txt")
      .getLines
      .toIndexedSeq

    val regex = raw"""(\d+)-(\d+) (\w{1}): (\w*)""".r

    val passwords = input.map{
      case regex(min, max, letter, value) => Password(OldPolicy(min, max, letter), value)
    }
//    passwords.foreach { p =>
//      println(s"$p => ${p.valid}")
//    }
    passwords.count(_.valid)
  }

  def partTwo: Int = {
    val input = Source.fromResource("input.txt")
      .getLines
      .toIndexedSeq

    val regex = raw"""(\d+)-(\d+) (\w{1}): (\w*)""".r

    val passwords = input.map{
      case regex(a, b, letter, value) => Password(NewPolicy(a, b, letter), value)
    }
//    passwords.foreach { p =>
//      println(s"$p => ${p.valid}")
//    }
    passwords.count(_.valid)
  }

  override def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}