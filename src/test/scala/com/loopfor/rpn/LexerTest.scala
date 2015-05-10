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

import org.scalatest.FunSuite
import scala.util.{Failure, Success}

class LexerTest extends FunSuite {
  import Tests._

  private val lexer = BasicLexer()

  test("valid tokens in randomized expressions") {
    for (_ <- 1 to 100) {
      val (expr, tokens) = Expression.generate()
      lexer(expr) match {
        case Success(ts) =>
          for ((x, y) <- ts zip tokens) assert(x.lexeme === y.lexeme)
        case Failure(e) => fail(e)
      }
    }
  }

  test("malformed numbers") {
    val tests = Seq(
          ".",
          ".1",
          "1.",
          "1.2."
          )

    for (expr <- tests) {
      lexer(expr) match {
        case Success(_) => fail(expr)
        case Failure(_) =>
      }
    }
  }
}
