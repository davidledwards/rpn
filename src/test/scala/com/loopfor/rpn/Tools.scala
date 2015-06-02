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

/**
 * Tools for automatically generating random conformant expressions.
 * 
 * Note that these expressions are generated for testing purposes only and may
 * appear to be senseless when visually inspected. However, they do conform to
 * the specified grammar and are sufficient for detecting regressions.
 * 
 * The output of each test generation is formatted such that it can be
 * literally cut and pasted into the unit test itself.
 */
object Tools {
  def main(args: Array[String]): Unit = {
    parserTests(100)
    generatorTests(100)
    optimizerTests(100)
  }

  /**
   * Produces a random expression and its corresponding AST.
   */
  def parserTests(count: Int): Unit = {
    import Tests._
    println(s"${tab(1)}private val tests = Seq[(String, AST)](")
    for (n <- 1 to count) {
      val (expr, _) = Expression.generate()
      println(s"""${tab(2)}("$expr",""")
      val ast = Parser(Lexer(expr))
      def emit(ast: AST, depth: Int): Unit = {
        def printAST(name: String, l: AST, r: AST): Unit = {
          println(s"${tab(depth)}$name(")
          emit(l, depth + 1)
          println(",")
          emit(r, depth + 1)
          print(s"\n${tab(depth)})")
        }
        ast match {
          case SymbolAST(n) =>
            print(s"""${tab(depth)}SymbolAST("$n")""")
          case NumberAST(v) =>
            print(s"""${tab(depth)}NumberAST($v)""")
          case AddAST(l, r) =>
            printAST("AddAST", l, r)
          case SubtractAST(l, r) =>
            printAST("SubtractAST", l, r)
          case MultiplyAST(l, r) =>
            printAST("MultiplyAST", l, r)
          case DivideAST(l, r) =>
            printAST("DivideAST", l, r)
          case ModuloAST(l, r) =>
            printAST("ModuloAST", l, r)
          case PowerAST(base, exp) =>
            printAST("PowerAST", base, exp)
          case MinAST(l, r) =>
            printAST("MinAST", l, r)
          case MaxAST(l, r) =>
            printAST("MaxAST", l, r)
        }
      }
      emit(ast, 3)
      print(")")
      if (n < count) println(",") else println(")")
    }
  }

  /**
   * Produces a random expression and its corresponding unoptimized instruction
   * sequence.
   */
  def generatorTests(count: Int): Unit = {
    import Tests._
    println(s"${tab(1)}private val tests = Seq[(String, Seq[Code])](")
    for (n <- 1 to count) {
      val (expr, _) = Expression.generate()
      println(s"""${tab(2)}("$expr",""")
      println(s"${tab(3)}Seq(")
      val codes = Generator(Parser(Lexer(expr)))
      @tailrec def emit(codes: Seq[Code]): Unit = codes match {
        case Seq(code, rest @ _*) =>
          val s = code match {
            case c: DeclareSymbolCode =>
              s"""DeclareSymbolCode("${c.name}")"""
            case c: PushSymbolCode =>
              s"""PushSymbolCode("${c.name}")"""
            case _ =>
              s"$code"
          }
          print(s"${tab(4)}$s")
          if (rest.isEmpty) print("))") else println(",")
          emit(rest)
        case _ =>
      }
      emit(codes)
      if (n < count) println(",") else println(")")
    }
  }

  /**
   * Produces a sequence of instructions with a computed value.
   * 
   * The computed value is possible due to a method of assigning values to each
   * symbol during evalation using a deterministic hash. Since symbol names are
   * stable, an optimization of the instruction sequence should produce the
   * same value.
   * 
   * Note that in some cases, the precomputed value is `NaN` or `Infinity`, which
   * should be removed from the set before use in unit tests.
   */
  def optimizerTests(count: Int): Unit = {
    import Tests._
    println(s"${tab(1)}private val tests = Seq[(Double, Seq[Code])](")
    for (n <- 1 to count) {
      val (expr, _) = Expression.generate()
      val codes = Generator(Parser(Lexer(expr)))
      val result = Evaluator(codes.toStream) { name => Some(hash(name)) }
      println(s"""${tab(2)}($result,""")
      println(s"${tab(3)}Seq(")
      @tailrec def emit(codes: Seq[Code]): Unit = codes match {
        case Seq(code, rest @ _*) =>
          val s = code match {
            case c: DeclareSymbolCode =>
              s"""DeclareSymbolCode("${c.name}")"""
            case c: PushSymbolCode =>
              s"""PushSymbolCode("${c.name}")"""
            case _ =>
              s"$code"
          }
          print(s"${tab(4)}$s")
          if (rest.isEmpty) print("))") else println(",")
          emit(rest)
        case _ =>
      }
      emit(codes)
      if (n < count) println(",") else println(")")
    }
  }

  def hash(name: String): Double = {
    name.foldLeft(0.0) { case (h, c) => h + c / 100.0 } / 10.0
  }

  private def tab(n: Int) = "  " * n
}
