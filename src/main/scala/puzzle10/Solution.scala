package puzzle10

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input10.txt")
    .getLines
    .toIndexedSeq

  def partOne: Int = {
    val sorted = input.map(_.toInt).sorted
    val map = sorted.foldLeft((Map.empty[Int, Int], 0)) {
      case ((map, last), curr) =>
        val count = map.getOrElse(curr - last, 0)
        (map ++ Map((curr - last) -> (count + 1)), curr)
    }._1
    map.getOrElse(1, 0) * (map.getOrElse(3, 0) + 1)
  }

  def popMap(curr: BigInt, remaining: Seq[BigInt], map: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
    val newMap = map ++ Map(curr ->
      map.filter {
        case (num, _) if curr - num <= 3 => true
        case _ => false
      }.values.sum)
    if (remaining.isEmpty) newMap
    else {
      popMap(remaining.head, remaining.tail, newMap)
    }
  }

  def partTwo: BigInt = {
    val sorted = input.map(BigInt(_)).sorted
    val map = popMap(sorted.head, sorted.tail, Map(BigInt(0) -> BigInt(1)))
    map.getOrElse(sorted.last, throw new Exception("Something's wrong!"))
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}