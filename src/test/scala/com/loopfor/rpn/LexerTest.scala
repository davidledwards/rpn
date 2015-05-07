package com.loopfor.rpn

import org.scalatest.FunSuite
import scala.io.Source
import scala.util.{Failure, Success}

class LexerTest extends FunSuite {
  private val lexer = BasicLexer()

  test("valid randomized expressions") {
    for (_ <- 1 to 100) {
      val (expr, tokens) = Expression.generate()
      val in = Source.fromString(expr).toStream
      lexer(in) match {
        case Success(ts) =>
          for ((x, y) <- ts zip tokens) assert(x.lexeme === y.lexeme)
        case Failure(e) => fail(e)
      }
    }
  }
}
