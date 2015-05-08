package com.loopfor.rpn

import org.scalatest.FunSuite
import scala.io.Source
import scala.util.{Failure, Success, Try}

class ParserTest extends FunSuite {
  import Tests._

  private val parser: Stream[Char] => Try[AST] = {
    val lex = BasicLexer()
    val par = BasicParser()
    in => lex(in) flatMap { par(_) }
  }

  test("valid randomized expressions") {
    for ((expr, ast) <- parserTests) {
      parser(expr) match {
        case Success(a) => assert(a === ast)
        case Failure(e) => fail(e)
      }
    }
  }

  test("invalid expressions") {
    val tests = Seq(
          "",
          " ",
          "a +",
          "+ a",
          "a 1",
          "(a + 1",
          "a + 1)",
          "(a + 1))",
          ")a",
          "()",
          "a + * 1",
          "a $ 1",
          "(a + 1)(b + 2)"
          )

    for (expr <- tests) {
      parser(expr) match {
        case Success(_) => fail(expr)
        case Failure(_) =>
      }
    }
  }
}
