package com.loopfor.rpn.compiler

import scala.annotation.tailrec

/**
 * Represents a syntax tree constructed by parsing a stream of tokens.
 */
sealed trait AST

case class SymbolAST(name: String) extends AST
case class NumberAST(value: Double) extends AST
case class AddAST(l: AST, r: AST) extends AST
case class SubtractAST(l: AST, r: AST) extends AST
case class MultiplyAST(l: AST, r: AST) extends AST
case class DivideAST(l: AST, r: AST) extends AST
case class ModuloAST(l: AST, r: AST) extends AST
case class PowerAST(base: AST, exp: AST) extends AST
case class MinAST(l: AST, r: AST) extends AST
case class MaxAST(l: AST, r: AST) extends AST

object AST {
  def format(ast: AST): String = {
    def format(ast: AST, depth: Int): String = {
      val text = ast match {
        case SymbolAST(n) => s"Symbol($n)\n"
        case NumberAST(v) => s"Number($v)\n"
        case AddAST(l, r) => "Add\n" + format(l, depth + 1) + format(r, depth + 1)
        case SubtractAST(l, r) => "Subtract\n" + format(l, depth + 1) + format(r, depth + 1)
        case MultiplyAST(l, r) => "Multiply\n" + format(l, depth + 1) + format(r, depth + 1)
        case DivideAST(l, r) => "Divide\n" + format(l, depth + 1) + format(r, depth + 1)
        case ModuloAST(l, r) => "Modulo\n" + format(l, depth + 1) + format(r, depth + 1)
        case PowerAST(base, exp) => "Power\n" + format(base, depth + 1) + format(exp, depth + 1)
        case MinAST(l, r) => "Min\n" + format(l, depth + 1) + format(r, depth + 1)
        case MaxAST(l, r) => "Max\n" + format(l, depth + 1) + format(r, depth + 1)
      }
      ("  " * depth) + text
    }
    format(ast, 0)
  }
}
