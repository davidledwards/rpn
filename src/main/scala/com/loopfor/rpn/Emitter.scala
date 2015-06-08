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

/**
 * An emitter that transforms a sequence of instructions into a stream of strings.
 */
trait Emitter extends (Seq[Code] => Stream[String])

private class BasicEmitter extends Emitter {
  def apply(codes: Seq[Code]): Stream[String] = {
    def emit(codes: Seq[Code]): Stream[String] = codes match {
      case Seq(code, rest @ _*) => code.repr #:: emit(rest)
      case Seq() => Stream.Empty
    }
    emit(codes)
  }
}

object Emitter {
  def apply(): Emitter = new BasicEmitter
  def apply(codes: Seq[Code]): Stream[String] = apply()(codes)
}
