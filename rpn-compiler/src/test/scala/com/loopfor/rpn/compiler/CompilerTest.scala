package com.loopfor.rpn.compiler

import scala.io.Source

object CompilerTest {
  def main(args: Array[String]): Unit = {
    val lexer = Lexer()
    val parser = Parser()
    val optimizer = Optimizer()
    val c = (lexer.apply _) andThen (parser.apply _) andThen (optimizer.apply _)

    val in = Source.fromString("1.2 + x * (3.32 / 4.981) - ((y * 6.1) + 7.0001) - z").toStream
    val ast = c(in)
    println(ast)
  }
}
