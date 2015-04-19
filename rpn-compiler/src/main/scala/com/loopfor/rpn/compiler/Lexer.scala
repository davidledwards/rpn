package com.loopfor.rpn.compiler

import scala.annotation.tailrec
import scala.collection.immutable.Stream

class Lexer private () {
  def apply(in: Stream[Char]): Stream[Token] = {
    def tokens(in: Stream[Char]): Stream[Token] = tokenize(in) match {
      case (EOSToken, _) => Stream.Empty
      case (token, rest) => token #:: tokens(rest)
    }
    tokens(in)
  }

  @tailrec private def tokenize(in: Stream[Char]): (Token, Stream[Char]) = in.headOption match {
    case Some(Space(_)) => tokenize(in.tail)
    case Some(Token(t)) => (t, in.tail)
    case Some(Digit(c)) => readNumber(in.tail, s"$c")
    case Some(Letter(c)) => readSymbol(in.tail, s"$c")
    case Some(c) => throw new Exception(s"$c: unrecognized character")
    case None => (EOSToken, in)
  }

  @tailrec private def readNumber(in: Stream[Char], lexeme: String): (Token, Stream[Char]) = in.headOption match {
    case Some('.') =>
      if (lexeme contains '.') throw new Exception(s"$lexeme.: malformed number")
      else readNumber(in.tail, lexeme + '.')
    case Some(Digit(c)) => readNumber(in.tail, lexeme + c)
    case _ =>
      if (lexeme.last == '.') throw new Exception(s"$lexeme: malformed number")
      else (NumberToken(lexeme), in)
  }

  @tailrec private def readSymbol(in: Stream[Char], lexeme: String): (Token, Stream[Char]) = in.headOption match {
    case Some(Letter(c)) => readSymbol(in.tail, lexeme + c)
    case _ =>
      (lexeme match {
        case Token(t) => t
        case _ => SymbolToken(lexeme)
      }, in)
  }

  private object Digit {
    private val digits = ('0' to '9').to[Set]
    def unapply(c: Char): Option[Char] = if (digits(c)) Some(c) else None
  }

  private object Letter {
    private val letters = (('A' to 'Z') ++ ('a' to 'z')).to[Set]
    def unapply(c: Char): Option[Char] = if (letters(c)) Some(c) else None
  }

  private object Space {
    private val whitespace: Set[Char] = Set(' ', '\n', '\r', '\t', '\f')
    def unapply(c: Char): Option[Char] = if (whitespace(c)) Some(c) else None
  }
}

object Lexer {
  def apply(): Lexer = new Lexer
}
