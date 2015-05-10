/*
 * Copyright 2015 David Edwards
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.loopfor.rpn

/**
 * Represents a token recognized in the input stream.
 * 
 * Recognized tokens defined as regular expressions:
 * {{{
 * Plus = +
 * Minus = -
 * Star = *
 * Slash = /
 * Percent = %
 * Caret = ^
 * LeftParen = (
 * RightParen = )
 * Min = min
 * Max = max
 * Symbol = [A-Za-z]+
 * Number = ([0-9]+)|([0-9]+\.[0-9]+)
 * EOS = <end of stream>
 * }}}
 */
sealed abstract class Token(val lexeme: String)

case object PlusToken extends Token("+")
case object MinusToken extends Token("-")
case object StarToken extends Token("*")
case object SlashToken extends Token("/")
case object PercentToken extends Token("%")
case object CaretToken extends Token("^")
case object LeftParenToken extends Token("(")
case object RightParenToken extends Token(")")
case object MinToken extends Token("min")
case object MaxToken extends Token("max")

case class SymbolToken(override val lexeme: String) extends Token(lexeme)
case class NumberToken(override val lexeme: String) extends Token(lexeme)

case object EOSToken extends Token("<EOS>")

object Token {
  private val simple: Map[Char, Token] = Map(
        PlusToken.lexeme(0) -> PlusToken,
        MinusToken.lexeme(0) -> MinusToken,
        StarToken.lexeme(0) -> StarToken,
        SlashToken.lexeme(0) -> SlashToken,
        PercentToken.lexeme(0) -> PercentToken,
        CaretToken.lexeme(0) -> CaretToken,
        LeftParenToken.lexeme(0) -> LeftParenToken,
        RightParenToken.lexeme(0) -> RightParenToken
        )

  private val symbols: Map[String, Token] = Map(
        CaretToken.lexeme -> CaretToken,
        MinToken.lexeme -> MinToken,
        MaxToken.lexeme -> MaxToken
        )

  def unapply(c: Char): Option[Token] = simple.get(c)
  def unapply(s: String): Option[Token] = symbols.get(s)
}
