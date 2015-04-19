package com.loopfor.rpn.compiler

sealed trait AST

case class SymbolAST(name: String) extends AST
case class NumberAST(value: Double) extends AST
case class AddAST(l: AST, r: AST) extends AST
case class SubtractAST(l: AST, r: AST) extends AST
case class MultiplyAST(l: AST, r: AST) extends AST
case class DivideAST(l: AST, r: AST) extends AST
case class ModuloAST(l: AST, r: AST) extends AST
case class PowerAST(base: AST, exp: AST) extends AST
case class RootAST(n: AST, root: AST) extends AST
case class MinAST(l: AST, r: AST) extends AST
case class MaxAST(l: AST, r: AST) extends AST
