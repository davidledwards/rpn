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
import scala.math.{max, min, pow}

/**
 * An evaluator that computes the result of an instruction sequence.
 */
trait Evaluator extends (Stream[Code] => Double)

private class BasicEvaluator(val resolver: String => Option[Double]) extends Evaluator {
  import Evaluator._

  def apply(codes: Stream[Code]): Double = {
    @tailrec def evaluate(codes: Stream[Code],
                          stack: Seq[Double],
                          syms: Map[String, Double]): Double = codes.headOption match {
      case Some(DeclareSymbolCode(name)) =>
        resolver(name) match {
          case Some(v) =>
            evaluate(codes.tail, stack, syms + (name -> v))
          case None =>
            throw new Exception(s"$name: symbol not bound")
        }
      case Some(PushSymbolCode(name)) =>
        (syms get name) match {
          case Some(v) => evaluate(codes.tail, v +: stack, syms)
          case None => throw new Exception(s"$name: symbol not bound")
        }
      case Some(PushCode(v)) =>
        evaluate(codes.tail, v +: stack, syms)
      case Some(c: OperatorCode) =>
        val (vs, rest) = stack splitAt c.args
        if (vs.size == c.args)
          evaluate(codes.tail, (vs.reverse reduceLeft operators(c.getClass)) +: rest, syms)
        else
          throw new Exception(s"evaluator stack underflow")
      case Some(_) =>
        evaluate(codes.tail, stack, syms)
      case None =>
        stack match {
          case Seq(r) => r
          case _ => throw new Exception(s"evaluator stack size should be 1, but is ${stack.size}")
        }
    }
    evaluate(codes, Seq.empty, Map.empty)
  }
}

object Evaluator {
  def apply(resolver: String => Option[Double]): Evaluator = new BasicEvaluator(resolver)
  def apply(codes: Stream[Code])(resolver: String => Option[Double]): Double = apply(resolver)(codes)

  val operators: Map[Class[_ <: OperatorCode], (Double, Double) => Double] = Map(
    classOf[AddCode] -> { _ + _ },
    classOf[SubtractCode] -> { _ - _ },
    classOf[MultiplyCode] -> { _ * _ },
    classOf[DivideCode] -> { _ / _ },
    classOf[MinCode] -> { min(_, _) },
    classOf[MaxCode] -> { max(_, _) },
    ModuloCode.getClass -> { _ % _ },
    PowerCode.getClass -> { pow(_, _) }
    )
}
