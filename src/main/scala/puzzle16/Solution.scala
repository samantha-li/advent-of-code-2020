package puzzle16

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {

  val input: Seq[String] = Source.fromResource("input16.txt")
    .getLines
    .toSeq
  
  val indexOfMyTicket: Int = input.zipWithIndex.filter(x => x._1 == "your ticket:").head._2 + 1
  val indexOfNearbyTickets: Int = indexOfMyTicket + 3

  def getValidSet(input: Seq[String]): Set[Int] = {
    input.foldLeft(Set.empty[Int]) {
      case (acc, s"$rule: ${int(lo1)}-${int(hi1)} or ${int(lo2)}-${int(hi2)}") =>
        acc ++ (lo1 to hi1).toSet ++ (lo2 to hi2).toSet
    }
  }

  def checkValid(validSet: Set[Int], tickets: Seq[String], acc: Int): Int = {
    tickets match {
      case Seq() => acc
      case hd::tl =>
        checkValid(
          validSet,
          tl,
          acc + hd.split(',').map(_.toInt).toSet.diff(validSet).sum
        )
    }
  }

  val validSet = getValidSet(input.splitAt(indexOfMyTicket)._1.dropRight(2))

  def partOne: Int = {
    checkValid(validSet, input.splitAt(indexOfNearbyTickets)._2, 0)
  }

  def getValid(validSet: Set[Int], tickets: Seq[String], acc: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    tickets match {
      case Seq() => acc
      case hd::tl =>
        val ticket = hd.split(',').map(_.toInt).toSeq
        val newAcc = if (ticket.toSet.diff(validSet).isEmpty)
          acc ++ Seq(ticket)
        else acc
        getValid(
          validSet,
          tl,
          newAcc
        )
    }
  }

  def getValidSetMap(input: Seq[String], validSetMap: Map[String, Set[Int]]): Map[String, Set[Int]] = {
    input.foldLeft(Map.empty[String, Set[Int]]) {
      case (acc, s"$field: ${int(lo1)}-${int(hi1)} or ${int(lo2)}-${int(hi2)}") =>
        acc ++ Map(field -> ((lo1 to hi1).toSet ++ (lo2 to hi2).toSet))
    }
  }

  def filterByTicket(ticket: Seq[Int], map: Map[Int, Set[String]], validSetMap: Map[String, Set[Int]]): Map[Int, Set[String]] = {
    ticket.zipWithIndex.foldLeft(map) {
      case (currMap, (num, idx)) =>
        val remaining = currMap(idx).intersect(validSetMap.filter(_._2.contains(num)).keySet)
        currMap ++ Map(idx -> remaining)
    }
  }

  def recurse(map: Seq[(Int, Set[String])], done: Map[Int, String]): Map[Int, String] = {
    if (map.isEmpty) done
    else {
      val sorted = map.sortBy(_._2.size)
      val (idx, field) = sorted.head
      recurse(sorted.tail.map {
        case (k, v) => (k, v.diff(field))
      }, done ++ Map(idx -> field.head))
    }
  }

  def getFinal(tickets: Seq[Seq[Int]], validSetMap: Map[String, Set[Int]]): Map[Int, String] = {
    val initMap = tickets.head.indices.map(_ -> validSetMap.keySet).toMap
    val map = tickets.foldLeft(initMap){
      case (m, t) => filterByTicket(t, m, validSetMap)
    }
    recurse(map.toSeq, Map.empty[Int, String])
  }

  def partTwo: BigInt = {
    val validTix = getValid(validSet, input.splitAt(indexOfNearbyTickets)._2, Seq.empty[Seq[Int]])
    val validSetMap = getValidSetMap(input.splitAt(indexOfMyTicket)._1.dropRight(2), Map.empty[String, Set[Int]])
    val map = getFinal(validTix, validSetMap)
    val myTicket = input.splitAt(indexOfMyTicket)._2.head.split(',').map(_.toInt).toIndexedSeq
    map.filter(_._2.contains("departure")).keySet.foldLeft(BigInt(1)) {
      case (p, idx) => BigInt(myTicket(idx)) * p
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}