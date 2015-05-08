package com.loopfor.rpn

import scala.util.Random

object Expression {
  import Random._

  def generate(ws: => String = Space.space): (String, Seq[Token]) = {
    def expression() = {
      (0 until nextInt(4)).foldLeft(operand()) { case (e, _) =>
        (e :+ Operator.generate()) ++ operand()
      }
    }

    def operand(): Seq[Token] = nextInt(3) match {
      case 0 => Seq(Num.generate())
      case 1 => Seq(Sym.generate())
      case 2 => LeftParenToken +: expression() :+ RightParenToken
    }

    val tokens = expression()
    val expr = (for (t <- tokens) yield t.lexeme + ws).mkString
    (expr, tokens)
  }

  object Sym {
    private val chars = ('a' to 'z') ++ ('A' to 'Z')
    def generate() = SymbolToken(s"${chars(nextInt(chars.size))}${chars(nextInt(chars.size))}")
  }

  object Num {
    def generate() = NumberToken(f"${nextDouble() * 100}%2.2f")
  }

  object Space {
    private val chars = Array(" ", "\n", "\r", "\t", "\f")
    def generate() = chars(nextInt(chars.size))
    val space = " "
  }

  object Operator {
    private val operators = Array[Token](
          PlusToken,
          MinusToken,
          StarToken,
          SlashToken,
          PercentToken,
          CaretToken,
          MinToken,
          MaxToken)

    def generate(): Token = operators(nextInt(operators.size))
  }
}
