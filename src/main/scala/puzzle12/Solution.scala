package puzzle12

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input12.txt")
    .getLines
    .toIndexedSeq

  val compass: Map[Char, Int] = Map('E' -> 0, 'S' -> 1, 'W' -> 2, 'N' -> 3)

  def move(amt: Int, dir: Int, x: Int, y: Int): (Int, Int) = {
    if (dir == 3) (x, y + amt)
    else if (dir == 1) (x, y - amt)
    else if (dir == 0) (x + amt, y)
    else if (dir == 2) (x - amt, y)
    else throw new Exception("Big nope")
  }

  def execute(insns: Seq[String], x: Int, y: Int, dir: Int): (Int, Int) = {
    if (insns.isEmpty) (x, y)
    else {
      val insn = insns.head.head
      val num = insns.head.tail.toInt
      val (newDir, moveDir, amt) = insn match {
        case 'R' => ((dir + num / 90) % 4, 0, 0)
        case 'L' => (((dir - num / 90) + 4) % 4, 0, 0)
        case 'F' => (dir, dir, num)
        case d =>  (dir, compass(d), num)
      }
      val (newNs, newEw) = move(amt, moveDir, x, y)
      execute(insns.tail, newNs, newEw, newDir)
    }
  }

  def partOne: Int = {
    val (x, y) = execute(input, 0, 0, 0)
    x.abs + y.abs
  }

  def executeNew(insns: Seq[String], waypoint: (Int, Int), ship: (Int, Int)): (Int, Int) = {
    if (insns.isEmpty) ship
    else {
      val insn = insns.head.head
      val num = insns.head.tail.toInt
      val (newWp, newShip) = insn match {
        case 'R' =>
          ((0 until (num / 90)).foldLeft(waypoint){
            case ((x, y), _) => (y, -1 * x)
          }, ship)
        case 'L' =>
          ((0 until (num / 90)).foldLeft(waypoint){
            case ((x, y), _) => (-1 * y, x)
         }, ship)
        case 'F' => (waypoint, (ship._1 + waypoint._1 * num, ship._2 + waypoint._2 * num))
        case d =>
          (move(num, compass(d), waypoint._1, waypoint._2), ship)
      }
      executeNew(insns.tail, newWp, newShip)
    }
  }

  def partTwo: Int = {
    val (x, y) = executeNew(input, (10, 1), (0, 0))
    x.abs + y.abs
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}