/*
 * Copyright 2015 David Edwards
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.loopfor.rpn

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

abstract class OperatorCode(op: String, val args: Int) extends Code(op) {
  def isAssociative: Boolean
  def isCommutative: Boolean
}

abstract class FixedOperatorCode(op: String, args: Int) extends OperatorCode(op, args) {
  val repr = op
}

abstract class DynamicOperatorCode(op: String, args: Int) extends OperatorCode(op, args) {
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
  val repr = {
    val v = (f"$value%.10f").reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    s"$op $v"
  }
}

object PushCode {
  val pattern = """\s*push\s+(-?\d+|-?\d+\.\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the sum, and pushes the
 * result onto the evaluation stack.
 */
case class AddCode(override val args: Int) extends DynamicOperatorCode("add", args) {
  val isAssociative = true
  val isCommutative = true
}

object AddCode {
  val pattern = """\s*add\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the difference, and pushes
 * the result onto the evaluation stack.
 */
case class SubtractCode(override val args: Int) extends DynamicOperatorCode("sub", args) {
  val isAssociative = false
  val isCommutative = false
}

object SubtractCode {
  val pattern = """\s*sub\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the product, and pushes
 * the result onto the evaluation stack.
 */
case class MultiplyCode(override val args: Int) extends DynamicOperatorCode("mul", args) {
  val isAssociative = true
  val isCommutative = true
}

object MultiplyCode {
  val pattern = """\s*mul\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the quotient, and pushes
 * the result onto the evaluation stack.
 */
case class DivideCode(override val args: Int) extends DynamicOperatorCode("div", args) {
  val isAssociative = false
  val isCommutative = false
}

object DivideCode {
  val pattern = """\s*div\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the minimum, and pushes
 * the result onto the evaluation stack.
 */
case class MinCode(override val args: Int) extends DynamicOperatorCode("min", args) {
  val isAssociative = true
  val isCommutative = true
}

object MinCode {
  val pattern = """\s*min\s+(\d+)\s*""".r
}

/**
 * Pops `args` operands from the evaluation stack, computes the maximum, and pushes
 * the result onto the evaluation stack.
 */
case class MaxCode(override val args: Int) extends DynamicOperatorCode("max", args) {
  val isAssociative = true
  val isCommutative = true
}

object MaxCode {
  val pattern = """\s*max\s+(\d+)\s*""".r
}

/**
 * Pops two operands from the evaluation stack, computes the remainder, and pushes
 * the result onto the evaluation stack.
 */
case object ModuloCode extends FixedOperatorCode("mod", 2) {
  val isAssociative = false
  val isCommutative = false
  val pattern = """\s*mod\s*""".r
}

/**
 * Pops two operands from the evaluation stack, computes the exponentiation, and pushes
 * the result onto the evaluation stack.
 */
case object PowerCode extends FixedOperatorCode("pow", 2) {
  val isAssociative = false
  val isCommutative = false
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
          case c: OperatorCode => 1 - c.args
          case _ => 0
        }
        (pos + 1, frame + f, out + s"$pos [$frame] ${c.repr}\n")
    }
    out
  }

  def symbols(codes: Seq[Code]): Seq[String] = {
    codes flatMap {
      case DeclareSymbolCode(name) => Seq(name)
      case _ => Seq()
    }
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
