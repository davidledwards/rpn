package com.loopfor.rpn.compiler

class Optimizer private () {
  def apply(ast: AST): AST = {
    ast
  }
}

object Optimizer {
  def apply(): Optimizer = new Optimizer
}
