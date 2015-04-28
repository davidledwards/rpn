package com.loopfor.rpn

import scala.io.Source
import scala.util.{Failure, Success}

object CompilerTest {
  def main(args: Array[String]): Unit = {
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
      println(s"\n$s ->")
      val in = Source.fromString(s).toStream
      BasicLexer()(in) flatMap { tokens =>
        BasicParser()(tokens)
      } flatMap { ast =>
        println("---syntax tree---")
        println(AST.format(ast))
        BasicGenerator()(ast)
      } flatMap { unopt =>
        println("---unoptimized---")
        println(Codes.format(unopt))
        BasicOptimizer()(unopt)
      } match {
        case Success(codes) =>
          println("---optimized---")
          println(Codes.format(codes))
        case Failure(e) =>
          println(e.getMessage)
      }
    }
  }
}
