package com.loopfor.rpn

import scala.io.Source
import scala.language._

object Tests {
  implicit def stringToStream(s: String): Stream[Char] = Source.fromString(s).toStream
}
