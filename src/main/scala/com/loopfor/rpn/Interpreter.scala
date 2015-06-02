/*
 * Copyright 2015 David Edwards
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.loopfor.rpn

import scala.annotation.tailrec
import scala.collection.immutable.Stream
import scala.io.Source

object Interpreter {
  def main(args: Array[String]): Unit = try {
    lazy val in = Source.stdin.toStream
    args.headOption match {
      case Some("-?") =>
        println("usage: rpn [option] [sym val]...")
        println("  Evaluate instructions from stdin and print result to stdout.")
        println("  Binds optional sequence of sym/val pairs prior to evaluation.")
        println("  -s  print symbols")
      case Some("-s") =>
        val codes = Loader(in)
        for (name <- Codes.symbols(codes)) println(name)
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
        val result = Evaluator(Loader(in)) { syms get _ }
        println(result)
    }
  } catch {
    case e: Exception => println(e.getMessage)
  }
}
