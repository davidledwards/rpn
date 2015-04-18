package com.loopfor.rpn.compiler

class Parser private () {
  def apply(in: Stream[Token]): Expr = {
    ???
  }

  private def parse(in: Stream[Token]): Expr = in.headOption match {
    case Some(LeftParenToken) => ???
  }

  private def p0(in: Stream[Token]): (Expr, Stream[Token]) = {
    val (l, rest) = p1(in)
    rest.headOption match {
      case Some(PlusToken) => p1(rest.tail)
    }
    ???
  }

  private def p0tail(in: Stream[Token]): (Expr, Stream[Token]) = {
    
  }

  private def p1(in: Stream[Token]): (Expr, Stream[Token]) = {
    p2(in)
  }

  private def p2(in: Stream[Token]): (Expr, Stream[Token]) = in.headOption match {
    case Some(LeftParenToken) =>
      val (expr, rest) = p0(in.tail)
      (expr, confirm(rest, RightParenToken))
    case Some(SymbolToken(lexeme)) =>
      (SymbolExpr(lexeme), in.tail)
    case Some(NumberToken(lexeme)) =>
      (NumberExpr(lexeme.toDouble), in.tail)
    case _ =>
      throw new Exception("expecting '(', <symbol> or <number>")
  }

  private def confirm(in: Stream[Token], token: Token): Stream[Token] = in.headOption match {
    case Some(t) if t == token => in.tail
    case _ => throw new Exception(s"expecting: '${token.lexeme}'")
  }
}

object Parser {
  def apply(): Parser = new Parser
}
