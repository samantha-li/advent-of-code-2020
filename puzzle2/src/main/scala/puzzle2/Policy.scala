package puzzle2

trait Policy {
  def validate(value: String): Boolean
}

case class OldPolicy(min: Int, max: Int, letter: Char) extends Policy {
  def validate(value: String): Boolean = {
    val count = value.count(_ == letter)
    count >= min && count <= max
  }
}

object OldPolicy {
  def apply(minStr: String, maxStr: String, letterStr: String): Policy = {
    OldPolicy(minStr.toInt, maxStr.toInt, letterStr(0))

  }
}

case class NewPolicy(a: Int, b: Int, letter: Char) extends Policy {
  def validate(value: String): Boolean = {
    (value.charAt(a - 1) == letter) ^ (value.charAt(b - 1) == letter)
  }
}

object NewPolicy {
  def apply(firstStr: String, secondStr: String, letterStr: String): Policy = {
    NewPolicy(firstStr.toInt, secondStr.toInt, letterStr(0))

  }
}