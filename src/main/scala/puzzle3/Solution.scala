package puzzle3

import scala.io.Source

object Solution {
  val input: IndexedSeq[String] = Source.fromResource("input3.txt")
    .getLines
    .toIndexedSeq

  val patternLength = input.head.length

  def partOne: Int = {
    input.foldLeft(Seq.empty[Boolean]) {
      case (locs, row) =>
        val drop = 3  * locs.length % patternLength
        Seq(row.drop(drop).head == '#') ++ locs
    }.count(tree => tree)
  }

  def partTwo: Int = {
    val inputMapped: Seq[(Int, String)] = input.indices.zip(input)
    val steps = Seq(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )
    steps.foldLeft(1){
      case (product, (right, down)) =>
        inputMapped.foldLeft(Seq.empty[Boolean]) {
          case (locs, (i, row)) =>
            if (i % down == 0) {
              val drop = right  * locs.length % patternLength
              Seq(row.drop(drop).head == '#') ++ locs
            } else locs
        }.count(tree => tree) * product
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}