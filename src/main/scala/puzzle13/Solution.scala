package puzzle13

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input13.txt")
    .getLines
    .toIndexedSeq

  def partOne: Int = {
    val time = input.head.toInt
    val buses = input(1).split(',').filter(_ != "x").map(_.toInt)
    val (best, waitTime) = buses.foldLeft((-1, Integer.MAX_VALUE)) {
      case ((currBest, waitTime), bus) =>
        if (time % bus == 0) (bus, 0)
        else if (bus - time % bus < waitTime) (bus, bus - time % bus)
        else (currBest, waitTime)
    }
    best * waitTime
  }

  def recurse(coef: BigInt, b: BigInt, i: BigInt, rem: BigInt, mod: BigInt): BigInt = {
    if ((coef * i + b) % mod != rem) recurse(coef, b, i + 1, rem, mod)
    else i
  }

  def partTwo: BigInt = {
    val congruences = input(1).split(',').zipWithIndex.filter(_._1 != "x").map{
      case (bus, idx) => (BigInt(bus), BigInt(idx))
    }.toSeq.sortBy(_._1).reverse
    val (coef, b) = congruences.tail.foldLeft(congruences.head){
      case ((coef, b), (mod, rem)) =>
        val solved = recurse(coef % mod, b % mod, 0, rem % mod, mod)
        (coef * mod, coef * solved + b)
    }
    coef % b
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}