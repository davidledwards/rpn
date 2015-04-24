package com.loopfor.rpn.compiler

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
  val repr = s"$op"
}

abstract class ScalarCode(op: String, val args: Int) extends Code(op)

abstract class FixedScalarCode(op: String, args: Int) extends ScalarCode(op, args) {
  val repr = s"$op"
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

/**
 * Pushes the symbol `name` onto the evaluation stack.
 */
case class PushSymbolCode(name: String) extends Code("pushsym") {
  val repr = s"$op $name"
}

/**
 * Pushes the number `value` onto the evaluation stack.
 */
case class PushCode(value: Double) extends Code("push") {
  val repr = s"$op $value"
}

/**
 * Pops `args` operands from the evaluation stack, computes the sum, and pushes the
 * result onto the evaluation stack.
 */
case class AddCode(override val args: Int) extends DynamicScalarCode("add", args)

/**
 * Pops `args` operands from the evaluation stack, computes the difference, and pushes
 * the result onto the evaluation stack.
 */
case class SubtractCode(override val args: Int) extends DynamicScalarCode("sub", args)

/**
 * Pops `args` operands from the evaluation stack, computes the product, and pushes
 * the result onto the evaluation stack.
 */
case class MultiplyCode(override val args: Int) extends DynamicScalarCode("mul", args)

/**
 * Pops `args` operands from the evaluation stack, computes the quotient, and pushes
 * the result onto the evaluation stack.
 */
case class DivideCode(override val args: Int) extends DynamicScalarCode("div", args)

/**
 * Pops `args` operands from the evaluation stack, computes the minimum, and pushes
 * the result onto the evaluation stack.
 */
case class MinCode(override val args: Int) extends DynamicScalarCode("min", args)

/**
 * Pops `args` operands from the evaluation stack, computes the maximum, and pushes
 * the result onto the evaluation stack.
 */
case class MaxCode(override val args: Int) extends DynamicScalarCode("max", args)

/**
 * Pops two operands from the evaluation stack, computes the remainder, and pushes
 * the result onto the evaluation stack.
 */
case object ModuloCode extends FixedScalarCode("mod", 2)

/**
 * Pops two operands from the evaluation stack, computes the exponentiation, and pushes
 * the result onto the evaluation stack.
 */
case object PowerCode extends FixedScalarCode("pow", 2)

/**
 * An instruction that has no effect.
 */
case object NopCode extends BasicCode("nop")

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
}
