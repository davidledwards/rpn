package com.loopfor.rpn

import scala.util.Try

/**
 * A code generator that transforms a syntax tree to a sequence of instructions.
 * 
 * The instruction sequence should be considered unoptimized.
 */
trait Generator {
  def apply(ast: AST): Try[Seq[Code]]
}

private class BasicGenerator extends Generator {
  def apply(ast: AST): Try[Seq[Code]] = Try {
    generate(ast)
  }

  private def generate(ast: AST): Seq[Code] = {
    // Represents sequence of instruction codes and symbol names.
    type State = (Seq[Code], Set[String])

    def generate(ast: AST, state: State): State = ast match {
      case SymbolAST(name) =>
        val (codes, syms) = state
        (PushSymbolCode(name) +: codes, syms + name)
      case NumberAST(value) =>
        val (codes, syms) = state
        (PushCode(value) +: codes, syms)
      case AddAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (AddCode(2) +: codes, syms)
      case SubtractAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (SubtractCode(2) +: codes, syms)
      case MultiplyAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (MultiplyCode(2) +: codes, syms)
      case DivideAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (DivideCode(2) +: codes, syms)
      case MinAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (MinCode(2) +: codes, syms)
      case MaxAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (MaxCode(2) +: codes, syms)
      case ModuloAST(l, r) =>
        val (codes, syms) = generate(r, generate(l, state))
        (ModuloCode +: codes, syms)
      case PowerAST(base, exp) =>
        val (codes, syms) = generate(exp, generate(base, state))
        (PowerCode +: codes, syms)
    }
    // Instruction codes are generated in reverse order and set of referenced
    // symbols is gather during traversal.
    val (codes, syms) = generate(ast, (Seq.empty, Set.empty))

    // Map set of symbol names to declarations and prepend to instruction codes.
    val decls = syms.toSeq.sorted map { name => DeclareSymbolCode(name) }
    decls ++: codes.reverse
  }
}

object BasicGenerator {
  def apply(): Generator = new BasicGenerator
  def apply(ast: AST): Try[Seq[Code]] = apply()(ast)
}
