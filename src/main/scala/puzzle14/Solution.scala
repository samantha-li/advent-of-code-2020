package puzzle14

import scala.io.Source
import scodec.bits._

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input14.txt")
    .getLines
    .toIndexedSeq

  def bitMaskToMap(str: String): Map[Int, Boolean] = {
    str.toCharArray.toSeq.zipWithIndex.foldLeft(Map.empty[Int, Boolean]) {
      case (map, (char, idx)) if char != 'X' => map ++ Map(idx -> (char == '1'))
      case (map, _) => map
    }
  }

  def partOne: Long = {
    val firstMask = input.head match {
      case s"mask = $bitMask" => bitMaskToMap(bitMask)
    }
    input.foldLeft((firstMask, Map.empty[Int, Long])) {
      case ((_, store), s"mask = $bitMask") => (bitMaskToMap(bitMask), store)
      case ((currMask, store), s"mem[$addr] = $num") =>
        val newNum: BitVector = currMask.foldLeft(BitVector.fromLong(num.toInt, 36)) {
          case (bv, (n, high)) => bv.update(n, high)
        }
        (currMask, store ++ Map(addr.toInt -> newNum.toLong(signed = false)))
    }._2.values.sum
  }

  def bitMaskToMapNew(str: String): Map[Int, Boolean] = {
    str.toCharArray.toSeq.zipWithIndex.foldLeft(Map.empty[Int, Boolean]) {
      case (map, (char, idx)) if char != '0' => map ++ Map(idx -> (char == '1'))
      case (map, _) => map
    }
  }

  def recurse(baseAddr: BitVector, floating: Set[Int], agg: Set[BitVector]): Set[BitVector] = {
    val currLength = agg.head.size
    if (currLength == 36) agg
    else if (floating.contains(currLength.toInt)) {
      val newAgg = agg.flatMap { bv =>
        val zero = bv.padRight(currLength + 1)
        val one = zero.update(currLength, high = true)
        Seq(zero, one)
      }
      recurse(baseAddr, floating, newAgg)

    } else {
      val newAgg = agg.map { bv =>
        val zero = bv.padRight(currLength + 1)
        if (!baseAddr.get(currLength)) zero
        else zero.update(currLength, high = true)
      }
      recurse(baseAddr, floating, newAgg)
    }
  }

  def getAddresses(addr: BitVector, bitMask: Map[Int, Boolean]): Set[Long] = {
    val modified = bitMask.filter(_._2).foldLeft(addr) {
      case (bv, (n, high)) => bv.update(n, high)
    }
    recurse(modified, bitMask.filter(!_._2).keys.toSet, Set(BitVector.empty)).map(_.toLong(signed = false))
  }

  def partTwo: BigInt = {
    val firstMask = input.head match {
      case s"mask = $bitMask" => bitMaskToMapNew(bitMask)
    }
    input.foldLeft((firstMask, Map.empty[Long, Long])) {
      case ((_, store), s"mask = $bitMask") => (bitMaskToMapNew(bitMask), store)
      case ((currMask, store), s"mem[$addr] = $num") =>
        val addresses = getAddresses(
          BitVector.fromLong(addr.toLong, 36),
          currMask
        )
        (
          currMask,
          addresses.foldLeft(store) {
            case (s, a) => s ++ Map(a -> num.toLong)
          }
        )
    }._2.values.sum
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}