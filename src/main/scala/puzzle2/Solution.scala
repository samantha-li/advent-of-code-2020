package puzzle2

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input2.txt")
    .getLines
    .toSeq

  def partOne: Int = {
    val passwords = input.map {
      case s"$min-$max $letter: $value" => Password(OldPolicy(min, max, letter), value)
    }

    passwords.count(_.valid)
  }

  def partTwo: Int = {
    val input = Source.fromResource("input2.txt")
      .getLines
      .toIndexedSeq

    val regex = raw"""(\d+)-(\d+) (\w{1}): (\w*)""".r

    val passwords = input.map{
      case regex(a, b, letter, value) => Password(NewPolicy(a, b, letter), value)
    }

    passwords.count(_.valid)
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}