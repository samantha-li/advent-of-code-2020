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
    value.toIntOption.exists(v => v >= 1920 && v <= 2002)
  }
}

case object IssueYear extends Field {
  val fieldName = "iyr"
  def validate(value: String): Boolean = {
    value.toIntOption.exists(v => v >= 2010 && v <= 2020)
  }
}

case object ExpirationYear extends Field {
  val fieldName = "eyr"
  def validate(value: String): Boolean = {
    value.toIntOption.exists(v => v >= 2020 && v <= 2030)
  }
}

case object Height extends Field {
  val fieldName = "hgt"
  def validate(value: String): Boolean = {
    value match {
      case s"${num}cm" => num.toIntOption.exists(v => v >= 150 && v <= 193)
      case s"${num}in" => num.toIntOption.exists(v => v >= 59 && v <= 76)
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