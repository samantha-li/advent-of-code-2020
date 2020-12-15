package puzzle15

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: Seq[Int] = Source.fromResource("input15.txt")
    .getLines
    .toSeq
    .head
    .split(',')
    .map(_.toInt)

  def play(idx: Int, last: Int, hist: Map[Int, Int], endIdx: Int): Int = {
    val curr = idx - 1 - hist.getOrElse(last, idx - 1)
    if (idx == endIdx) curr
    else play(idx + 1, curr, hist ++ Map(last -> (idx - 1)), endIdx)
  }

  val startingMap = input.zipWithIndex.foldLeft(Map.empty[Int, Int]) {
    case (map, (num, idx)) => map ++ Map(num -> (idx + 1))
  }

  def partOne: Int = play(input.length + 1, input.last, startingMap, 2020)

  def partTwo: Int = play(input.length + 1, input.last, startingMap, 30000000)

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}