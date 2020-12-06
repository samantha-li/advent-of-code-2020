package puzzle6

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input6.txt")
    .getLines
    .toSeq

  def partOne: Int = {
    val (currSum, curr) = input.foldLeft((0, Set.empty[Char])) {
      case ((sum, curr), line) => if (line.isEmpty) (sum + curr.size, Set.empty[Char])
      else (sum, curr ++ line.toSet)
    }
    currSum + curr.size
  }

  def process(input: Seq[String], sum: Int, curr: Set[Char]): Int = {
    input match {
      case Seq() => sum + curr.size
      case hd::tail if hd.isEmpty => process(tail.tail, sum + curr.size, tail.head.toSet)
      case hd::tail => process(tail, sum, curr.intersect(hd.toSet))
    }
  }

  def partTwo: Int = {
    process(input.tail, 0, input.head.toSet)
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}