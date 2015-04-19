package com.loopfor.rpn.compiler

import scala.io.Source

object CompilerTest {
  def main(args: Array[String]): Unit = {
    val lexer = Lexer()
    val parser = Parser()
    val optimizer = Optimizer()
    val c = (lexer.apply _) andThen (parser.apply _) andThen (optimizer.apply _)

    val in = Source.fromString("1.2 + x pow y * (3.32 / 4.981 + y root (1 / x)) - ((y * 6.1) + 7.0001 + (x min y) % 4.23) - z % (t * 0.123)").toStream
    val ast = c(in)
    println(ast)
  }
}
