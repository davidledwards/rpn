package com.loopfor.rpn.interpreter

import com.loopfor.rpn.compiler.Code
import scala.collection.immutable.Stream

class Loader private () {
  def apply(in: Stream[Char]): Seq[Code] = {
    ???
  }
}

object Loader {
  def apply(): Loader = new Loader
}
