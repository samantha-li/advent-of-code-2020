package puzzle4

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Solution {
  val input: IndexedSeq[String] = Source.fromResource("input4.txt")
    .getLines
    .toIndexedSeq

  val requiredFields: Set[Field] = Set(
    EyeColor,
    BirthYear,
    ExpirationYear,
    IssueYear,
    Height,
    HairColor,
    PassportId
  )

  val optionalFields = Set(
    "cid"
  )

  def partOne: Int = {
    val rFields = requiredFields.map(_.fieldName)
    val tuple = input.foldLeft((Seq.empty[Set[String]], Set.empty[String])){
      case ((seen, curr), line) =>
        val f = line.split(' ').toSeq.filterNot(_ == "").map {
          case s"$f:$v" => f
        }
        if (f.isEmpty) {
          (Seq(curr) ++ seen, Set.empty[String])
        }
        else (seen, f.toSet ++ curr)
    }
    val sets: Seq[Set[String]] = Seq(tuple._2) ++ tuple._1
    sets.count { s =>
      rFields.diff(s).isEmpty
    }
  }

  def partTwo: Int = {
    val (count, curr) = input.foldLeft((0, requiredFields)){
      case ((count, curr), line) =>
        val fs = line.split(' ').toSeq.filterNot(_ == "")
        if (fs.isEmpty) {
          if (curr.isEmpty) (count + 1, requiredFields)
          else (count, requiredFields)
        } else {
          val remaining = fs.foldLeft(curr) {
            case (c, s"$f:$v") =>
              if (Field(f).validate(v)) c.filterNot(_.fieldName == f)
              else c
          }
          (count, remaining)
        }
    }
    if (curr.isEmpty) count + 1 else count
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}