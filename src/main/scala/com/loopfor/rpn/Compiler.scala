package com.loopfor.rpn

import scala.collection.immutable.Stream
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Compiler {
  def main(args: Array[String]): Unit = {
    lazy val in = Source.stdin.toStream
    args.headOption match {
      case Some("-?") =>
        println("usage: rpnc [options]")
        println("  Compile expression from stdin and emit instructions to stdout.")
        println("  -t  tokenize only")
        println("  -p  parse only")
        println("  -o  optimize")
      case Some("-t") =>
        BasicLexer(in) match {
          case Success(tokens) => for (token <- tokens) println(token)
          case Failure(e) => println(e.getMessage)
        }
      case Some("-p") =>
        (for {
          tokens <- BasicLexer(in)
          ast <- BasicParser(tokens)
        } yield ast) match {
          case Success(ast) => println(AST.format(ast))
          case Failure(e) => println(e.getMessage)
        }
      case Some("-o") =>
        (for {
          tokens <- BasicLexer(in)
          ast <- BasicParser(tokens)
          unopt <- BasicGenerator(ast)
          opt <- BasicOptimizer(unopt)
        } yield opt) match {
          case Success(codes) => for (code <- codes) println(code.repr)
          case Failure(e) => println(e.getMessage)
        }
      case Some(arg) =>
        println(s"$arg: unrecognized option")
      case None =>
        (for {
          tokens <- BasicLexer(in)
          ast <- BasicParser(tokens)
          unopt <- BasicGenerator(ast)
          opt <- DisabledOptimizer(unopt)
        } yield opt) match {
          case Success(codes) => for (code <- codes) println(code.repr)
          case Failure(e) => println(e.getMessage)
        }
    }
  }
}
