package puzzle7

import scala.io.Source
import scala.util.matching.Regex

object Solution {
  val input: IndexedSeq[String] = Source.fromResource("input7.txt")
    .getLines
    .toIndexedSeq

  def findOne(str: String, map: Map[String, Seq[String]], set: Set[String]): Set[String] = {
    map.get(str).fold(set){
      _.foldLeft(set) {
        case (s, parent) => findOne(parent, map, s ++ Set(parent))
      }
    }
  }

  def partOne: Int = {
    val regex = new Regex(""" ?[0-9]+ ([a-z ]+) bags?""", "color")
    val map = input.foldLeft(Map.empty[String, Seq[String]]) {
      case (map, s"$color bags contain $bags.") =>
        bags.split(',').toSeq.foldLeft(map){
          case (m, str) if regex.matches(str) =>
            val child = regex.findAllIn(str).group("color")
            m ++ Map(child -> (m.getOrElse(child, Seq.empty) ++ Seq(color)))
          case (m,  "no other bags") => m
        }
    }
    findOne("shiny gold", map, Set.empty[String]).size
  }

  def findTwo(str: String, map: Map[String, Seq[(Int, String)]], count: Int): Int = {
    map.get(str).fold(count){
      _.foldLeft(count) {
        case (sum,  (c, color)) => sum + c * findTwo(color, map, 1)
      }
    }
  }

  def partTwo: Int = {
    val regex = new Regex(""" ?([0-9]+) ([a-z ]+) bags?""", "num", "color")
    val map = input.foldLeft(Map.empty[String, Seq[(Int, String)]]) {
      case (map, s"$color bags contain $bags.") =>
        map ++  Map(color -> bags.split(',').toSeq.foldLeft(Seq.empty[(Int, String)]){
          case (s, str) if regex.matches(str) =>
            val matchData = regex.findAllIn(str)
            val num =  matchData.group("num").toInt
            val child = matchData.group("color")
            s ++ Seq((num, child))
          case (s,  "no other bags") => s
        })
    }

    findTwo("shiny gold", map, 0)
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}