package com.loopfor.rpn.compiler

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

case class DeclareSymbolCode(name: String) extends Code("sym") {
  val repr = s"$op $name"
}

case class PushSymbolCode(name: String) extends Code("pushsym") {
  val repr = s"$op $name"
}

case class PushCode(value: Double) extends Code("push") {
  val repr = s"$op $value"
}

case class AddCode(override val args: Int) extends DynamicScalarCode("add", args)
case class SubtractCode(override val args: Int) extends DynamicScalarCode("sub", args)
case class MultiplyCode(override val args: Int) extends DynamicScalarCode("mul", args)
case class DivideCode(override val args: Int) extends DynamicScalarCode("div", args)
case class MinCode(override val args: Int) extends DynamicScalarCode("min", args)
case class MaxCode(override val args: Int) extends DynamicScalarCode("max", args)

case object ModuloCode extends FixedScalarCode("mod", 2)
case object PowerCode extends FixedScalarCode("pow", 2)

case object NopCode extends BasicCode("nop")

object Codes {
  def format(codes: Seq[Code]): String = {
    val (_, _, out) = ((0, 0, "") /: codes) { case ((pos, frame, out), c) =>
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
