package com.loopfor.rpn.compiler

import scala.util.matching.Regex

/**
 * Represents a bytecode instruction generated from a syntax tree.
 * 
 * Recognized instructions:
 * {{{
 * sym <symbol>
 * pushsym <symbol>
 * push <number>
 * add <args>
 * sub <args>
 * mul <args>
 * div <args>
 * min <args>
 * max <args>
 * mod
 * pow
 * nop
 * }}}
 */
sealed abstract class Code(val op: String) {
  def repr: String
}

abstract class BasicCode(op: String) extends Code(op) {
  val repr = op
}

abstract class ScalarCode(op: String, val args: Int) extends Code(op)

abstract class FixedScalarCode(op: String, args: Int) extends ScalarCode(op, args) {
  val repr = op
}

abstract class DynamicScalarCode(op: String, args: Int) extends ScalarCode(op, args) {
  val repr = s"$op $args"
}

/**
 * Declares a symbol before its use, which allows the interpreter to bind a value.
 */
case class DeclareSymbolCode(name: String) extends Code("sym") {
  val repr = s"$op $name"
}

object DeclareSymbolCode {
  val pattern = """\s*sym\s+([a-zA-Z]+)\s*""".r
}

/**
 * Pushes the symbol `name` onto the evaluation stack.
 */
case class PushSymbolCode(name: String) extends Code("pushsym") {
  val repr = s"$op $name"
}

object PushSymbolCode {
  val pattern = """\s*pushsym\s+([a-zA-Z]+)\s*""".r
}

/**
 * Pushes the number `value` onto the evaluation stack.
 */
case class PushCode(value: Double) extends Code("push") {
  val repr = s"$op $value"
}

object PushCode {
  val pattern = """\s*push\s+(\d+|\d+\.\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the sum, and pushes the
 * result onto the evaluation stack.
 */
case class AddCode(override val args: Int) extends DynamicScalarCode("add", args)

object AddCode {
  val pattern = """\s*add\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the difference, and pushes
 * the result onto the evaluation stack.
 */
case class SubtractCode(override val args: Int) extends DynamicScalarCode("sub", args)

object SubtractCode {
  val pattern = """\s*sub\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the product, and pushes
 * the result onto the evaluation stack.
 */
case class MultiplyCode(override val args: Int) extends DynamicScalarCode("mul", args)

object MultiplyCode {
  val pattern = """\s*mul\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the quotient, and pushes
 * the result onto the evaluation stack.
 */
case class DivideCode(override val args: Int) extends DynamicScalarCode("div", args)

object DivideCode {
  val pattern = """\s*div\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the minimum, and pushes
 * the result onto the evaluation stack.
 */
case class MinCode(override val args: Int) extends DynamicScalarCode("min", args)

object MinCode {
  val pattern = """\s*min\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the maximum, and pushes
 * the result onto the evaluation stack.
 */
case class MaxCode(override val args: Int) extends DynamicScalarCode("max", args)

object MaxCode {
  val pattern = """\s*max\s+(\d+)\s*""".r
}

/**
 * Pops two operands from the evaluation stack, computes the remainder, and pushes
 * the result onto the evaluation stack.
 */
case object ModuloCode extends FixedScalarCode("mod", 2) {
  val pattern = """\s*mod\s*""".r
}

/**
 * Pops two operands from the evaluation stack, computes the exponentiation, and pushes
 * the result onto the evaluation stack.
 */
case object PowerCode extends FixedScalarCode("pow", 2) {
  val pattern = """\s*pow\s*""".r
}

/**
 * An instruction that has no effect.
 */
case object NopCode extends BasicCode("nop") {
  val pattern = """\s*nop\s*""".r
}

object Codes {
  def format(codes: Seq[Code]): String = {
    val (_, _, out) = codes.foldLeft((0, 0, "")) {
      case ((pos, frame, out), c) =>
        val f = c match {
          case PushSymbolCode(_) => 1
          case PushCode(_) => 1
          case c: ScalarCode => 1 - c.args
          case _ => 0
        }
        (pos + 1, frame + f, out + s"$pos [$frame] ${c.repr}\n")
    }
    out
  }

  def parse(repr: String): Option[Code] = repr match {
    case DeclareSymbolCode.pattern(name) => Some(DeclareSymbolCode(name))
    case PushSymbolCode.pattern(name) => Some(PushSymbolCode(name))
    case PushCode.pattern(value) =>
      try Some(PushCode(value.toDouble)) catch {
        case _: NumberFormatException => None
      }
    case AddCode.pattern(args) => verify(args, 2) { AddCode(_) }
    case SubtractCode.pattern(args) => verify(args, 2) { SubtractCode(_) }
    case MultiplyCode.pattern(args) => verify(args, 2) { MultiplyCode(_) }
    case DivideCode.pattern(args) => verify(args, 2) { DivideCode(_) }
    case MinCode.pattern(args) => verify(args, 2) { MinCode(_) }
    case MaxCode.pattern(args) => verify(args, 2) { MaxCode(_) }
    case ModuloCode.pattern(_*) => Some(ModuloCode)
    case PowerCode.pattern(_*) => Some(PowerCode)
    case NopCode.pattern(_*) => Some(NopCode)
    case _ => None
  }

  private def verify(args: String, lower: Int)(fn: Int => Code): Option[Code] = {
    try {
      val n = args.toInt
      if (n < lower) None else Some(fn(n))
    } catch {
      case _: NumberFormatException => None
    }
  }
}
