package com.loopfor.rpn

import scala.util.Random

object Expression {
  private val whitespace = Array(" ", "\n", "\r", "\t", "\f")
  private val chars = ('a' to 'z') ++ ('A' to 'Z')
  private val operators = Array[Token](
        PlusToken,
        MinusToken,
        StarToken,
        SlashToken,
        PercentToken,
        PowerToken,
        MinToken,
        MaxToken)

  def generate(): (String, Seq[Token]) = {
    import Random._

    def expression() = {
      (0 until nextInt(4)).foldLeft(operand()) { case (e, _) =>
        val op = operators(nextInt(operators.size))
        (e :+ op) ++ operand()
      }
    }

    def operand(): Seq[Token] = nextInt(3) match {
      case 0 => Seq(NumberToken(f"${nextDouble() * 100}%2.2f"))
      case 1 => Seq(SymbolToken(s"${chars(nextInt(chars.size))}${chars(nextInt(chars.size))}"))
      case 2 => LeftParenToken +: expression() :+ RightParenToken
    }

    def space() = whitespace(nextInt(whitespace.size))

    val tokens = expression()
    val expr = (for (t <- tokens) yield t.lexeme + space()).mkString
    (expr, tokens)
  }
}