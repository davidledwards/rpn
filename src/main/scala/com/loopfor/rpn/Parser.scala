package com.loopfor.rpn

import scala.annotation.tailrec
import scala.util.Try

/**
 * A recursive-descent parser that transforms a stream of tokens into a syntax tree.
 * 
 * Grammar:
 * {{{
 * p0 ::= <p2> <p1>
 * p1 ::= '+' <p2> <p1>
 *    ::= '-' <p2> <p1>
 *    ::= e
 * p2 ::= <p4> <p3>
 * p3 ::= '*' <p4> <p3>
 *    ::= '/' <p4> <p3>
 *    ::= '%' <p4> <p3>
 *    ::= '^' <p4> <p3>
 *    ::= e
 * p4 ::= <p6> <p5>
 * p5 ::= 'min' <p6> <p5>
 *    ::= 'max' <p6> <p5>
 *    ::= e
 * p6 ::= '(' <p0> ')'
 *    ::= <symbol>
 *    ::= <number>
 * }}}
 */
class Parser private () {
  def apply(in: Stream[Token]): Try[AST] = Try {
    val (ast, rest) = p0(in)
    rest.headOption match {
      case Some(t) => throw new Exception(s"${t.lexeme}: expecting ${EOSToken.lexeme}")
      case None => ast
    }
  }

  /**
   * p0 ::= <p2> <p1>
   */
  private def p0(in: Stream[Token]): (AST, Stream[Token]) = {
    val (l, rest) = p2(in)
    p1(l, rest)
  }

  /**
   * p1 ::= '+' <p2> <p1>
   *    ::= '-' <p2> <p1>
   *    ::= e
   */
  @tailrec private def p1(l: AST, in: Stream[Token]): (AST, Stream[Token]) = in.headOption match {
    case Some(PlusToken) =>
      val (r, rest) = p2(in.tail)
      p1(AddAST(l, r), rest)
    case Some(MinusToken) =>
      val (r, rest) = p2(in.tail)
      p1(SubtractAST(l, r), rest)
    case _ => (l, in)
  }

  /**
   * p2 ::= <p4> <p3>
   */
  private def p2(in: Stream[Token]): (AST, Stream[Token]) = {
    val (l, rest) = p4(in)
    p3(l, rest)
  }

  /**
   * p3 ::= '*' <p4> <p3>
   *    ::= '/' <p4> <p3>
   *    ::= '%' <p4> <p3>
   *    ::= '^' <p4> <p3>
   *    ::= e
   */
  @tailrec private def p3(l: AST, in: Stream[Token]): (AST, Stream[Token]) = in.headOption match {
    case Some(StarToken) =>
      val (r, rest) = p4(in.tail)
      p3(MultiplyAST(l, r), rest)
    case Some(SlashToken) =>
      val (r, rest) = p4(in.tail)
      p3(DivideAST(l, r), rest)
    case Some(PercentToken) =>
      val (r, rest) = p4(in.tail)
      p3(ModuloAST(l, r), rest)
    case Some(PowerToken) =>
      val (r, rest) = p4(in.tail)
      p3(PowerAST(l, r), rest)
    case _ => (l, in)
  }

  /**
   * p4 ::= <p6> <p5>
   */
  private def p4(in: Stream[Token]): (AST, Stream[Token]) = {
    val (l, rest) = p6(in)
    p5(l, rest)
  }

  /**
   * p5 ::= 'min' <p6> <p5>
   *    ::= 'max' <p6> <p5>
   *    ::= e
   */
  @tailrec private def p5(l: AST, in: Stream[Token]): (AST, Stream[Token]) = in.headOption match {
    case Some(MinToken) =>
      val (r, rest) = p6(in.tail)
      p5(MinAST(l, r), rest)
    case Some(MaxToken) =>
      val (r, rest) = p6(in.tail)
      p5(MaxAST(l, r), rest)
    case _ => (l, in)
  }

  /**
   * p6 ::= '(' <p0> ')'
   *    ::= <symbol>
   *    ::= <number>
   */
  private def p6(in: Stream[Token]): (AST, Stream[Token]) = in.headOption match {
    case Some(LeftParenToken) =>
      val (ast, rest) = p0(in.tail)
      (ast, confirm(rest, RightParenToken))
    case Some(SymbolToken(lexeme)) => (SymbolAST(lexeme), in.tail)
    case Some(NumberToken(lexeme)) => (NumberAST(lexeme.toDouble), in.tail)
    case t @ _ =>
      val lexeme = (t getOrElse EOSToken).lexeme
      throw new Exception(s"$lexeme: expecting '(', <symbol> or <number>")
  }

  private def confirm(in: Stream[Token], token: Token): Stream[Token] = in.headOption match {
    case Some(t) if t == token => in.tail
    case t @ _ =>
      val lexeme = (t getOrElse EOSToken).lexeme
      throw new Exception(s"$lexeme: expecting '${token.lexeme}'")
  }
}

object Parser {
  def apply(): Parser = new Parser
}
