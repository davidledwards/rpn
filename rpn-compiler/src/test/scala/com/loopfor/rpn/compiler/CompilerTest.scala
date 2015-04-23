package com.loopfor.rpn.compiler

import scala.io.Source

object CompilerTest {
  def main(args: Array[String]): Unit = {
    val lexer = Lexer()
    val parser = Parser()
    val generator = Generator()
    val optimizer = Optimizer()
    val c = (lexer.apply _) andThen (parser.apply _)
    val cg = c andThen (generator.apply _) andThen (optimizer.apply _)

    val examples = Seq(
          "1.2 + x ^ y * (3.32 / 4.981 + y ^ (1 / x)) - ((y * 6.1) + 7.0001 + (x min y) % 4.23) - z % (t * 0.123)",
          "1 + 2 + 3",
          "(1 + 2) + 3",
          "1 + (2 + 3)",
          "1 + 2 * 3",
          "1 + 2 + 3 + 4",
          "1 + (2 * 3) + 4",
          "x min y min z",
          "1 + (2 * 3 * 5) + (6 / 7 / 8) + (9 min 10 min 11 min 12) + (x + y + z)",
          "1 + (x + 2 + 3) + y + 4",
          "(x + 5) + y + 5",
          "2 ^ 3",
          "8 ^ (1 / 3)"
          )

    examples foreach { s =>
      try {
        val in = Source.fromString(s).toStream
        val ast = c(in)
        println(s"\n$s ->")
        println(AST.format(ast))
        val codes = generator(ast)
        codes foreach { c => println(c.repr) }
        val optimized = optimizer(codes)
        println("---optimized---")
        optimized foreach { c => println(c.repr) }
      } catch {
        case e: Exception => println(s"$s -> ${e.getMessage}")
      }
    }
  }
}
