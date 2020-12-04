package puzzle4

import scala.util.{Failure, Success, Try}

sealed trait Field {
  val fieldName: String
  def validate(value: String): Boolean
}

object Field {
  def apply(value: String): Field = {
    value match {
      case EyeColor.fieldName => EyeColor
      case BirthYear.fieldName => BirthYear
      case ExpirationYear.fieldName => ExpirationYear
      case IssueYear.fieldName => IssueYear
      case Height.fieldName => Height
      case HairColor.fieldName => HairColor
      case PassportId.fieldName => PassportId
      case CountryId.fieldName => CountryId
    }
  }
}

case object BirthYear extends Field {
  val fieldName = "byr"
  def validate(value: String): Boolean = {
    Try(value.toInt).map(1920 to 2002 contains _).getOrElse(false)
  }
}

case object IssueYear extends Field {
  val fieldName = "iyr"
  def validate(value: String): Boolean = {
    Try(value.toInt).map(2010 to 2020 contains _).getOrElse(false)
  }
}

case object ExpirationYear extends Field {
  val fieldName = "eyr"
  def validate(value: String): Boolean = {
    Try(value.toInt).map(2020 to 2030 contains _).getOrElse(false)
  }
}

case object Height extends Field {
  val fieldName = "hgt"
  def validate(value: String): Boolean = {
    value match {
      case s"${num}cm" => Try(num.toInt).map(150 to 193 contains _).getOrElse(false)
      case s"${num}in" => Try(num.toInt).map(59 to 76 contains _).getOrElse(false)
      case _ => false
    }
  }
}

case object HairColor extends Field {
  val fieldName = "hcl"
  def validate(value: String): Boolean = {
    value.matches("#[0-9a-f]{6}")
  }
}

case object EyeColor extends Field {
  val fieldName = "ecl"
  val colors = Set(
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth"
  )
  def validate(value: String): Boolean = {
    colors.contains(value)
  }
}

case object PassportId extends Field {
  val fieldName = "pid"
  def validate(value: String): Boolean = {
    value.matches("[0-9]{9}")
  }
}

case object CountryId extends Field {
  val fieldName = "cid"
  def validate(value: String): Boolean = true
}