package com.loopfor.rpn

import org.scalatest.FunSuite
import scala.io.Source
import scala.util.{Failure, Success, Try}

class GeneratorTest extends FunSuite {
  import Tests._

  private val generator: Stream[Char] => Try[Seq[Code]] = {
    val lex = BasicLexer()
    val par = BasicParser()
    val gen = BasicGenerator()
    in => lex(in) flatMap { par(_) } flatMap { gen(_) }
  }

  test("valid expressions") {
    for ((expr, codes) <- generatorTests) {
      generator(expr) match {
        case Success(cs) =>
          for ((x, y) <- cs zip codes) assert(x.op === y.op)
        case Failure(e) => fail(e)
      }
    }
  }
}
