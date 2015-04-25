package com.loopfor.rpn.interpreter

import com.loopfor.rpn.compiler.{Code, Codes}
import scala.annotation.tailrec
import scala.collection.immutable.Stream

class Loader private () {
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
  def apply(): Loader = new Loader
}
