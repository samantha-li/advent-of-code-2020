package puzzle9

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input9.txt")
    .getLines
    .toSeq

  def recurseWindow(target: BigInt, window: Seq[BigInt]): Boolean = {
    window match {
      case Seq() => false
      case hd::tl if tl.contains(target - hd) => true
      case _::tl => recurseWindow(target, tl)
    }
  }

  def recurse(window: Seq[BigInt], input: Seq[BigInt]): BigInt = {
    if (recurseWindow(input.head, window)) recurse(window.tail ++ Seq(input.head), input.tail)
    else input.head
  }

  def partOne: BigInt = {
    val splits = input.map(BigInt(_)).splitAt(25)
    recurse(splits._1, splits._2)
  }

  def sliding(target: BigInt, window: Seq[BigInt], input: Seq[BigInt]): BigInt = {
    val sum = window.sum
    if (window.length < 2 || sum < target) sliding(target, window ++ Seq(input.head), input.tail)
    else if (sum == target) {
      val sorted = window.sorted
      sorted.last + sorted.head
    }
    else sliding(target, window.tail, input)
  }

  def partTwo(target: BigInt): BigInt = {
    sliding(target, Seq.empty[BigInt], input.map(BigInt(_)))
  }

  def main(args: Array[String]): Unit = {
    val answer1 = partOne
    println(answer1)
    println(partTwo(answer1))
  }
}