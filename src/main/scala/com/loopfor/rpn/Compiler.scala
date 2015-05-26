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
          lines <- BasicEmitter(opt)
        } yield lines) match {
          case Success(lines) => for (l <- lines) println(l)
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
          lines <- BasicEmitter(opt)
        } yield lines) match {
          case Success(lines) => for (l <- lines) println(l)
          case Failure(e) => println(e.getMessage)
        }
    }
  }
}
