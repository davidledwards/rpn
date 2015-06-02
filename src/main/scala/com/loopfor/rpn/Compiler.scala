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

import scala.collection.immutable.Stream
import scala.io.Source

object Compiler {
  def main(args: Array[String]): Unit = try {
    lazy val in = Source.stdin.toStream
    args.headOption match {
      case Some("-?") =>
        println("usage: rpnc [options]")
        println("  Compile expression from stdin and emit instructions to stdout.")
        println("  -t  tokenize only")
        println("  -p  parse only")
        println("  -o  optimize")
      case Some("-t") =>
        val tokens = Lexer(in)
        for (token <- tokens) println(token)
      case Some("-p") =>
        val ast = Parser(Lexer(in))
        println(AST.format(ast))
      case Some("-o") =>
        val lines = Emitter(Optimizer(Generator(Parser(Lexer(in)))))
        for (line <- lines) println(line)
      case Some(arg) =>
        println(s"$arg: unrecognized option")
      case None =>
        val lines = Emitter(Optimizer.disabled(Generator(Parser(Lexer(in)))))
        for (line <- lines) println(line)
    }
  } catch {
    case e: Exception => println(e.getMessage)
  }
}
