package com.loopfor.rpn.compiler

import scala.io.Source

object CompilerTest {
  def main(args: Array[String]): Unit = {
    val lexer = Lexer()
    val in = Source.fromString("1.2 + 2.75 * (3.32 / 4.981) - ((5.345 * 6.1) + 7.0001)").toStream
    val tokens = lexer(in)
    tokens foreach { println _ }
  }
}
