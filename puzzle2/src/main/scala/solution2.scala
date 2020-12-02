import scala.io.Source

object Day01 extends App {
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

  case class Password(policy: Policy, value: String) {
    def valid: Boolean = {
      policy.validate(value)
    }
  }

  def partOne: Int = {
    val input = Source.fromResource("input.txt")
      .getLines
      .toIndexedSeq

    val regex = raw"""(\d+)-(\d+) (\w{1}): (\w*)""".r

    val passwords = input.map{
      case regex(min, max, letter, value) => Password(OldPolicy(min, max, letter), value)
    }
//    passwords.foreach { p =>
//      println(s"$p => ${p.valid}")
//    }
    passwords.count(_.valid)
  }

  def partTwo: Int = {
    val input = Source.fromResource("input.txt")
      .getLines
      .toIndexedSeq

    val regex = raw"""(\d+)-(\d+) (\w{1}): (\w*)""".r

    val passwords = input.map{
      case regex(a, b, letter, value) => Password(NewPolicy(a, b, letter), value)
    }
//    passwords.foreach { p =>
//      println(s"$p => ${p.valid}")
//    }
    passwords.count(_.valid)
  }

  override def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}