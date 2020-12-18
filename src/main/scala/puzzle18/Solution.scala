package puzzle18

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

object Solution {
//  val input = IndexedSeq("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  val input: IndexedSeq[String] = Source.fromResource("input18.txt")
    .getLines
    .toIndexedSeq

  sealed trait Operator extends Element {
    val op: String
  }

  case object Multiply extends Operator { val op = "*" }
  case object Add extends Operator { val op = "+" }
  case object Noop extends Operator { val op = "noop" }
//  case object Open extends Operator { val op = "(" }
//  case object Close extends Operator { val op = ")" }

  trait Element
  case class Number(value: BigInt) extends Element
  case class Expression(el1: Element,
                        operator: Operator,
                        el2: Element) extends Element
  case object Empty extends Element

  def eval(element: Element): BigInt = {
    element match {
      case Number(num) => num
      case Expression(el1, _, el2) if el1 == Empty => eval(el2)
      case Expression(el1, _, el2) if el2 == Empty => eval(el1)
      case Expression(el1, op, el2) => op match {
        case Multiply => eval(el1) * eval(el2)
        case Add => eval(el1) + eval(el2)
        case Noop => eval(el2)
      }
    }
  }

  def parseInput(acc: Element, op: Operator, input: String): (Element, String) = {
    input match {
      case "" => (acc, "")
      case s"${int(num)} $remaining" => parseInput(Expression(acc, op, Number(BigInt(num))), Noop, remaining)
      case s"${int(num)}" => (Expression(acc, op, Number(BigInt(num))), "")
      case s"* $remaining" => parseInput(acc, Multiply, remaining)
      case s"+ $remaining" => parseInput(acc, Add, remaining)
      case s"( $remaining" =>
        val (elem, str) = parseInput(Empty, Noop, remaining)
        parseInput(Expression(acc, op, elem), Noop, str)
      case s") $remaining" => (acc, remaining)
      case s")" => (acc, "")
    }
  }

  def partOne: BigInt = {
    val replaced = input.map( _.replace("(", "( ").replace(")", " )") )
    replaced.foldLeft(BigInt(0)){
      case (acc, exp) =>
        val elements = parseInput(Empty, Noop, exp)
        acc + eval(elements._1)
    }
  }

  def evalStack(stack: Seq[Either[Operator, Element]], op: Operator, el: Element): Element = {
    stack match {
      case Seq() => el
      case hd::tl => hd match {
        case Left(op) => evalStack(tl, op, el)
        case Right(leftEl) => evalStack(tl, Noop, Expression(leftEl, op, el))
      }
    }
  }

  def parseInputS(stack: Seq[Either[Operator, Element]], input: String): (Element, String) = {
    input match {
      case "" => (evalStack(stack, Noop, Empty), "")
      case s"${int(num)} $remaining" =>
        stack match {
          case Left(Add)::Right(el)::tl => parseInputS(Seq(Right(Expression(el, Add, Number(BigInt(num))))) ++ tl, remaining)
          case _ => parseInputS(Seq(Right(Number(BigInt(num)))) ++ stack, remaining)
        }
      case s"${int(num)}" => (evalStack(Seq(Right(Number(BigInt(num)))) ++ stack, Noop, Empty), "")
      case s"* $remaining" => parseInputS(Seq(Left(Multiply)) ++ stack, remaining)
      case s"+ $remaining" => parseInputS(Seq(Left(Add)) ++ stack, remaining)
      case s"( $remaining" =>
        val (elem, str) = parseInputS(Seq.empty[Either[Operator, Element]], remaining)
        stack match {
          case Left(Add)::Right(el)::tl => parseInputS(Seq(Right(Expression(el, Add, elem))) ++ tl, str)
          case _ => parseInputS(Seq(Right(elem)) ++ stack, str)
        }
      case s") $remaining" => (evalStack(stack, Noop, Empty), remaining)
      case s")" => (evalStack(stack, Noop, Empty), "")
    }
  }

  def partTwo: BigInt = {
    val replaced = input.map( _.replace("(", "( ").replace(")", " )") )
    replaced.foldLeft(BigInt(0)){
      case (acc, exp) =>
        val elements = parseInputS(Seq.empty[Either[Operator, Element]], exp)
        acc + eval(elements._1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}