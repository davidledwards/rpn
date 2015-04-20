package com.loopfor.rpn.compiler

sealed abstract class Code(val op: String) {
  def repr: String
}

abstract class BasicCode(op: String) extends Code(op) {
  val repr = s"$op"
}

abstract class ScalarCode(op: String, val args: Int) extends Code(op) {
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

case class AddCode(override val args: Int) extends ScalarCode("add", args)
case class SubtractCode(override val args: Int) extends ScalarCode("sub", args)
case class MultiplyCode(override val args: Int) extends ScalarCode("mul", args)
case class DivideCode(override val args: Int) extends ScalarCode("div", args)
case class MinCode(override val args: Int) extends ScalarCode("min", args)
case class MaxCode(override val args: Int) extends ScalarCode("max", args)

case object ModuloCode extends BasicCode("mod")
case object PowerCode extends BasicCode("pow")
case object RootCode extends BasicCode("root")
case object NopCode extends BasicCode("nop")
