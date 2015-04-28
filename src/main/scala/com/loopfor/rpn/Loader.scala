package com.loopfor.rpn

import scala.annotation.tailrec
import scala.collection.immutable.Stream
import scala.util.Try

/**
 * An instruction loader that transforms a stream of characters into a stream of
 * instructions.
 */
trait Loader {
  def apply(in: Stream[Char]): Try[Stream[Code]]
}

private class BasicLoader extends Loader {
  def apply(in: Stream[Char]): Try[Stream[Code]] = Try {
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

object BasicLoader {
  def apply(): Loader = new BasicLoader
  def apply(in: Stream[Char]): Try[Stream[Code]] = apply()(in)
}
