package puzzle11

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input11.txt")
    .getLines
    .toIndexedSeq

  val length = input.head.length
  val height = input.length
  val totalLocs = length * height

  def mkString(occupied: Set[Int], floor: Set[Int]): String = {
    (0 until totalLocs).foldLeft("") {
      case (s, loc) =>
        val c = if (occupied.contains(loc)) '#'
          else if (floor.contains(loc)) '.'
          else 'L'
        if ((loc + 1) % length == 0) s + c + '\n'
        else s + c
    }
  }

  def flipSeats(occupied: Set[Int], floor: Set[Int]): Set[Int] = {
    val newOccupied = (0 until totalLocs).foldLeft(Set.empty[Int]) {
      case (newOccupancy, loc) if !floor.contains(loc) =>
        val empty = !occupied.contains(loc)
        val x = loc % length
        val y = loc / length
        val adjOccupancies = occupied.count {
         l =>
            val ax = l % length
            val ay = l / length
            (ax - x).abs <= 1 && (ay - y).abs <= 1
        }
        if (empty && adjOccupancies == 0) newOccupancy ++ Set(loc)
        else if (!empty && adjOccupancies < 5) newOccupancy ++ Set(loc)
        else newOccupancy
      case (newOccupancy, _) => newOccupancy
    }
    if (newOccupied.equals(occupied)) newOccupied
    else flipSeats(newOccupied, floor)
  }

  val floor = input.flatMap(_.toSeq).zipWithIndex.foldLeft(Set.empty[Int]) {
    case (floor, (c, idx)) if c == '.' => floor ++ Set(idx)
    case (floor, _) => floor
  }

  def partOne: Int = flipSeats(Set.empty[Int], floor).size

  def flipSeatsB(occupied: Set[Int], map: Map[Int, Set[Int]]): Set[Int] = {
    println(mkString(occupied, floor))
    val newOccupied = (0 until totalLocs).foldLeft(Set.empty[Int]) {
      case (newOccupancy, loc) if map.keySet.contains(loc) =>
        val empty = !occupied.contains(loc)
        val first = map.getOrElse(loc, throw new Exception("what the hell"))
        val adjOccupancies = occupied.count(first.contains)
        if (empty && adjOccupancies == 0) newOccupancy ++ Set(loc)
        else if (!empty && adjOccupancies < 5) newOccupancy ++ Set(loc)
        else newOccupancy
      case (newOccupancy, _) => newOccupancy
    }
    if (newOccupied.equals(occupied)) newOccupied
    else flipSeatsB(newOccupied, map)
  }

  def traverse(x: Int, y: Int, input: IndexedSeq[Char], xStep: Int, yStep: Int): Option[Int] = {
    val loc = y * length + x
    if (x < 0 || x >= length || y < 0 || y >= height)
      None
    else if (input(loc) == '.')
      traverse(x + xStep, y + yStep, input, xStep, yStep)
    else Some(loc)
  }

  def findAdj(loc: Int, input: IndexedSeq[Char]): Set[Int] = {
    val x = loc % length
    val y = loc / length
    Seq(
      (-1, -1),
      (0, -1),
      (1, -1),
      (-1, 0),
      (1, 0),
      (-1, 1),
      (0, 1),
      (1, 1)
    ).flatMap {
      case (xStep, yStep) =>
        traverse(x + xStep, y + yStep, input, xStep, yStep)
    }.toSet
  }

  def partTwo: Int = {
    val charSeq = input.flatMap(_.toSeq)
    val adjMap = charSeq.zipWithIndex.foldLeft(Map.empty[Int, Set[Int]]) {
      case (acc, (c, idx)) if c != '.' =>
        val set = findAdj(idx, charSeq)
        println(s"$idx -> $set")
        acc ++ Map(idx -> set)
      case (acc, _) => acc
    }
    flipSeatsB(Set.empty[Int], adjMap).size
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}