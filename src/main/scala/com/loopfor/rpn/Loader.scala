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

/**
 * An instruction loader that transforms a stream of characters into a stream of
 * instructions.
 */
trait Loader extends (Stream[Char] => Stream[Code])

private class BasicLoader extends Loader {
  def apply(in: Stream[Char]): Stream[Code] = {
    def load(in: Stream[Char]): Stream[Code] = read(in) match {
      case (null, _) => Stream.empty
      case (repr, rest) =>
        Codes.parse(repr) match {
          case Some(code) => code #:: load(rest)
          case None => throw new Exception(s"$repr: unrecognized or malformed instruction")
        }
    }
    load(in)
  }

  private def read(in: Stream[Char]): (String, Stream[Char]) = {
    @tailrec def slurp(in: Stream[Char], repr: String): (String, Stream[Char]) = in.headOption match {
      case Some('\n') => (repr, in.tail)
      case Some(c) => slurp(in.tail, repr + c)
      case None => (if (repr.size == 0) null else repr, in)
    }
    slurp(in, "")
  }
}

object Loader {
  def apply(): Loader = new BasicLoader
  def apply(in: Stream[Char]): Stream[Code] = apply()(in)
}
