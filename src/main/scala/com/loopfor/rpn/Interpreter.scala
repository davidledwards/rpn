package com.loopfor.rpn

import scala.annotation.tailrec
import scala.collection.immutable.Stream
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Interpreter {
  def main(args: Array[String]): Unit = {
    lazy val in = Source.stdin.toStream
    args.headOption match {
      case Some("-?") =>
        println("usage: rpn [option] [sym val]...")
        println("  Evaluate instructions from stdin and print result to stdout.")
        println("  Binds optional sequence of sym/val pairs prior to evaluation.")
        println("  -s  print symbols")
      case Some("-s") =>
        BasicLoader(in) match {
          case Success(codes) => for (name <- Codes.symbols(codes)) println(name)
          case Failure(e) => println(e.getMessage)
        }
      case Some(arg) if (arg startsWith "-") =>
        println(s"$arg: unrecognized option")
      case _ =>
        @tailrec def bind(args: Seq[String], syms: Map[String, Double]): Map[String, Double] = args match {
          case Seq(s, v, rest @ _*) =>
            bind(rest, try syms + (s -> v.toDouble) catch {
              case _: NumberFormatException =>
                println(s"$s <- $v: discarding symbol since value is malformed")
                syms
            })
          case Seq(s) =>
            println(s"$s: discarding symbol since value is missing")
            syms
          case Seq() => syms
        }
        val syms = bind(args, Map.empty)
        (for {
          codes <- BasicLoader(in)
          result <- BasicEvaluator(codes) { syms get _ }
        } yield result) match {
          case Success(r) => println(r)
          case Failure(e) => println(e.getMessage)
        }
    }
  }
}
