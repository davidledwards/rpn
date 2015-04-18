package com.loopfor.rpn.compiler

sealed abstract class Token(val lexeme: String)

case object PlusToken extends Token("+")
case object MinusToken extends Token("-")
case object StarToken extends Token("*")
case object SlashToken extends Token("/")
case object LeftParenToken extends Token("(")
case object RightParenToken extends Token(")")

case class SymbolToken(override val lexeme: String) extends Token(lexeme)
case class NumberToken(override val lexeme: String) extends Token(lexeme)

case object EOSToken extends Token("<EOS>")

object Token {
  private val simple: Map[Char, Token] = Map(
        '+' -> PlusToken,
        '-' -> MinusToken,
        '*' -> StarToken,
        '/' -> SlashToken,
        '(' -> LeftParenToken,
        ')' -> RightParenToken
        )

  def unapply(c: Char): Option[Token] = simple.get(c)
}
