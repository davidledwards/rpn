package com.loopfor.rpn.compiler

class Parser private () {
  def apply(in: Stream[Token]): Expr = {
    val (expr, rest) = p0(in)
    rest.headOption match {
      case Some(t) =>
        throw new Exception(s"${t.lexeme}: expecting ${EOSToken.lexeme}")
      case None =>
        expr
    }
  }

  private def p0(in: Stream[Token]): (Expr, Stream[Token]) = {
    val (l, rest) = p1(in)
    p0tail(l, rest)
  }

  private def p0tail(l: Expr, in: Stream[Token]): (Expr, Stream[Token]) = in.headOption match {
    case Some(PlusToken) =>
      val (r, rest) = p1(in.tail)
      p0tail(AddExpr(l, r), rest)
    case Some(MinusToken) =>
      val (r, rest) = p1(in.tail)
      p0tail(SubtractExpr(l, r), rest)
    case _ =>
      (l, in)
  }

  private def p1(in: Stream[Token]): (Expr, Stream[Token]) = {
    val (l, rest) = p2(in)
    p1tail(l, rest)
  }

  private def p1tail(l: Expr, in: Stream[Token]): (Expr, Stream[Token]) = in.headOption match {
    case Some(StarToken) =>
      val (r, rest) = p2(in.tail)
      p1tail(MultiplyExpr(l, r), rest)
    case Some(SlashToken) =>
      val (r, rest) = p2(in.tail)
      p1tail(DivideExpr(l, r), rest)
    case _ =>
      (l, in)
  }

  private def p2(in: Stream[Token]): (Expr, Stream[Token]) = in.headOption match {
    case Some(LeftParenToken) =>
      val (expr, rest) = p0(in.tail)
      (expr, confirm(rest, RightParenToken))
    case Some(SymbolToken(lexeme)) =>
      (SymbolExpr(lexeme), in.tail)
    case Some(NumberToken(lexeme)) =>
      (NumberExpr(lexeme.toDouble), in.tail)
    case t @ _ =>
      val lexeme = (t getOrElse EOSToken).lexeme
      throw new Exception(s"$lexeme: expecting '(', <symbol> or <number>")
  }

  private def confirm(in: Stream[Token], token: Token): Stream[Token] = in.headOption match {
    case Some(t) if t == token =>
      in.tail
    case t @ _ =>
      val lexeme = (t getOrElse EOSToken).lexeme
      throw new Exception(s"$lexeme: expecting: '${token.lexeme}'")
  }
}

object Parser {
  def apply(): Parser = new Parser
}
