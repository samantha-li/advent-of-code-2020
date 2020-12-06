package puzzle5

import scala.io.Source

object Solution {
  val input: IndexedSeq[String] = Source.fromResource("input5.txt")
    .getLines
    .toIndexedSeq

  val rowCount = 128
  val seatCount = 8

  val charMap = Map(
    'F' -> 0,
    'B' -> 1,
    'L' -> 0,
    'R' -> 1
  )

  def partOne: Int = {
    input.map { line =>
      line.tail.foldLeft(charMap(line.head)) {
        case (num, c) => num * 2 + charMap(c)
      }
    }.max
  }

  def binarySearchHelper(input: IndexedSeq[Int], hi: Int, lo: Int, offset: Int): (Int, Int) = {
    if (hi == lo + 1) (hi, lo)
    else {
      val index = (hi - lo) / 2 + lo
      if (input(index) > index + offset) binarySearchHelper(input, index, lo, offset)
      else if (input(index) == index + offset) binarySearchHelper(input, hi, index, offset)
      else throw new Exception("should not happen")
    }
  }

  def partTwo: Int = {
    val sorted = input.map { line =>
      line.tail.foldLeft(charMap(line.head)) {
        case (num, c) => num * 2 + charMap(c)
      }
    }.sorted
    val offset = sorted.head
    val (hi, _) = binarySearchHelper(sorted, sorted.length - 1, 0, offset)
    sorted(hi) - 1
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}