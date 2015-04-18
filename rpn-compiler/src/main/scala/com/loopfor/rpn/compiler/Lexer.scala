package com.loopfor.rpn.compiler

import scala.annotation.tailrec
import scala.collection.immutable.Stream

class Lexer private () {
  def apply(in: Stream[Char]): Stream[Token] = {
    def tokens(in: Stream[Char]): Stream[Token] = {
      tokenize(in) match {
        case (EOSToken, _) => Stream.Empty
        case (token, rest) => token #:: tokens(rest)
      }
    }
    tokens(in)
  }

  @tailrec private def tokenize(in: Stream[Char]): (Token, Stream[Char]) = {
    if (in.isEmpty) (EOSToken, in)
    else in.head match {
      case Space(_) => tokenize(in.tail)
      case Token(t) => (t, in.tail)
      case Digit(c) => tokenizeNumber(in.tail, s"$c")
      case Letter(c) => tokenizeSymbol(in.tail, s"$c")
      case c => throw new Exception(s"$c: unrecognized character")
    }
  }

  @tailrec private def tokenizeNumber(in: Stream[Char], lexeme: String): (NumberToken, Stream[Char]) = in.headOption match {
    case Some('.') =>
      if (lexeme contains '.') throw new Exception(s"$lexeme.: malformed number")
      else tokenizeNumber(in.tail, lexeme + '.')
    case Some(Digit(c)) =>
      tokenizeNumber(in.tail, lexeme + c)
    case _ =>
      if (lexeme.last == '.') throw new Exception(s"$lexeme: malformed number")
      else (NumberToken(lexeme), in)
  }

  @tailrec private def tokenizeSymbol(in: Stream[Char], lexeme: String): (SymbolToken, Stream[Char]) = in.headOption match {
    case Some(Letter(c)) => tokenizeSymbol(in.tail, lexeme + c)
    case _ => (SymbolToken(lexeme), in)
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
