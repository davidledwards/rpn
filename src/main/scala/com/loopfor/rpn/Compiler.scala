package com.loopfor.rpn

import scala.io.Source
import scala.util.{Failure, Success}

object Compiler {
  def main(args: Array[String]): Unit = {
    val lexer = Lexer()
    val parser = Parser()
    val generator = Generator()
    val optimizer = Optimizer()
    val in = Source.stdin.toStream
    val r = for {
      tokens <- lexer(in)
      ast <- parser(tokens)
      unopt <- generator(ast)
      opt <- optimizer(unopt)
    } yield opt
    r match {
      case Success(codes) =>
        for (code <- codes) println(code.repr)
      case Failure(e) =>
        println(e.getMessage)
    }
  }
}
