package com.loopfor.rpn

import org.scalatest.FunSuite
import scala.util.{Failure, Success}

class LexerTest extends FunSuite {
  import Tests._

  private val lexer = BasicLexer()

  test("valid tokens in randomized expressions") {
    for (_ <- 1 to 100) {
      val (expr, tokens) = Expression.generate()
      lexer(expr) match {
        case Success(ts) =>
          for ((x, y) <- ts zip tokens) assert(x.lexeme === y.lexeme)
        case Failure(e) => fail(e)
      }
    }
  }

  test("malformed numbers") {
    val tests = Seq(
          ".",
          ".1",
          "1.",
          "1.2."
          )

    for (expr <- tests) {
      lexer(expr) match {
        case Success(_) => fail(expr)
        case Failure(_) =>
      }
    }
  }
}
