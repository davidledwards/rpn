package com.loopfor.rpn

import scala.collection.immutable.Stream
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Interpreter {
  def main(args: Array[String]): Unit = {
    lazy val in = Source.stdin.toStream
    args.headOption match {
      case Some("-?") =>
        println("usage: rpn [options]")
        println("  Evaluate instructions from stdin and print result to stdout.")
        println("  -s  print symbols")
      case Some("-s") =>
        BasicLoader(in) match {
          case Success(codes) => for (name <- Codes.symbols(codes)) println(name)
          case Failure(e) => println(e.getMessage)
        }
      case Some(arg) =>
        println(s"$arg: unrecognized option")
      case None =>
        (for {
          codes <- BasicLoader(in)
          result <- BasicEvaluator(codes) { _ => 1.0 }
        } yield result) match {
          case Success(r) => println(r)
          case Failure(e) => println(e.getMessage)
        }
    }
  }
}
